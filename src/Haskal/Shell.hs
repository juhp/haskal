------------------------------------------------------------------------
-- |
-- Module      : Haskal.Shell
-- License     : GPL
--
------------------------------------------------------------------------
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of the
-- License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
-- 02110-1301, USA.

module Haskal.Shell
  ( Shell
  , JobID
  , JobStatus
  , runShell
  , startJob
  , awaitJob
  , isInteractive
  , prettyJobs
  , prettyJob
  , backgroundJob
  ) where

import Prelude hiding (catch)
import Control.Monad.State
import Control.Exception hiding (block)
import Data.Maybe
import Foreign.C.Types
import System.Posix.Types
import System.Posix.IO
import System.Posix.Signals
import System.Posix.Process
import System.Exit
import System.Posix.Terminal
import System.IO
import Text.PrettyPrint.HughesPJ
import Data.Typeable
import Data.List ( insert )

import Data.Map ( Map )
import qualified Data.Map as Map

import Haskal.Util


newtype Shell a = Shell (StateT ShellState IO a)
  deriving (Monad, MonadIO, MonadState ShellState)


runShell :: Shell a -> IO a
runShell m = evalShell undefined (initialise Nothing Nothing Nothing >> m)

evalShell :: ShellState -> Shell a -> IO a
evalShell ss (Shell m) = evalStateT m ss

newtype JobID = JobID Int
  deriving (Eq, Ord, Num, Integral, Enum, Real, Typeable)

instance Read JobID where
  readsPrec n = map (first JobID) . readsPrec n

instance Show JobID where
  show (JobID jid) = show jid

-- | The shell maintains state about the terminal as well as about the
-- jobs and processes. There's a map from process ids to processes so
-- that their status can be updated. Each process belongs to a job (a
-- number of processes whose inputs and outputs are piped together).
data ShellState
   = ShellState {
       -- | Input file descriptor for the shell
       shellInFd   :: Fd,
       -- | Output file descriptor for the shell
       shellOutFd  :: Fd,
       -- | Error file descriptor for the shell
       shellErrFd  :: Fd,
       -- | The process group id of the shell. If the shell is
       -- interactive, this is changed to the pid of the shell
       -- process, otherwise remains unchanged.
       shellPgid   :: ProcessID,
       -- | Stored terminal attributes used to restore the terminal
       -- after running jobs. Nothing if the shell is not interactive.
       shellAttrs  :: Maybe TerminalAttributes,
       -- | Map process ids to processes.
       shellProcs  :: Map ProcessID Process,
       -- | Map job ids to jobs.
       shellJobs   :: Map JobID Job,
       -- | Job id supply.
       shellJobIDs :: [JobID]
   } {-! derive : update !-}

-- | A single process.
data Process
   = Process {
      -- | The process id of the process.
      processID     :: ProcessID,
      -- | What job this process belongs to.
      processJob    :: JobID,
      -- | The status of this process.
      processStatus :: Maybe ProcessStatus
   } {-! derive : update !-}

-- | A job is a group of processes that are piped together. All
-- processes in a job are in the same process group so that they can
-- easily be signalled.
data Job
   = Job {
       -- | Unique id for this job.
       jobID      :: JobID,
       -- | The process group to which the processes in the job belongs.
       jobPgid    :: ProcessGroupID,
       -- | What command line was used to start the job.
       jobCmdLine :: String,
       -- | When the job is suspended the terminal attributes are stored.
       jobAttrs   :: Maybe TerminalAttributes,
       -- | What processes are part of the job.
       jobProcs   :: [(ProcessID, JobStatus)],
       -- | Derived from jobProcs.
       jobStatus  :: JobStatus
     } {-! derive : update !-}

-- | Denotes the state a job is in. Corresponds to ProcessStatus from
-- System.Posix.Processes
data JobStatus
  = JobDone       -- ^ Job has terminated normally.
  | JobTerminated -- ^ Job was killed.
  | JobStopped    -- ^ Job is stopped.
  | JobRunning    -- ^ Job is running.
    deriving (Show, Eq)

-- | Initialise the shell.
initialise :: Maybe Fd -> Maybe Fd -> Maybe Fd -> Shell ()
initialise mInFd mOutFd mErrFd = do
  -- Initial shell state
  pgid <- liftIO $ getProcessGroupID
  put $ ShellState
    { shellInFd   = fromMaybe stdInput mInFd
    , shellOutFd  = fromMaybe stdOutput mOutFd
    , shellErrFd  = fromMaybe stdError mErrFd
    , shellPgid   = pgid
    , shellAttrs  = Nothing
    , shellJobs   = Map.empty
    , shellProcs  = Map.empty
    , shellJobIDs = [1..]
    }
  -- If we run interactively we need to make sure we control the terminal.
  whenM isatty $ do
    let loop = do pgid' <- liftIO $ getProcessGroupID
                  tpgid <- tcgetpgrp
                  -- We wait to be put in the foreground
                  when (pgid' /= tpgid) $ do
                    liftIO $ signalProcessGroup backgroundRead pgid'
                    loop
    loop

    -- Ignore signals having to do with job control.
    mapM_ (\sig -> liftIO $ installHandler sig Ignore Nothing)
          jobControlSignals

    -- Change the shell's process group and take control over the terminal.
    pid <- liftIO $ getProcessID
    modify $ shellPgid_s pid
    liftIO $ setProcessGroupID pid pid
    tcsetpgrp pid

    -- Store the old terminal state so that it can be restored later.
    tcgetattr >>= modify . shellAttrs_s . Just



-- | 'True' if the in file descriptor of the shell is a tty.
isInteractive :: Shell Bool
isInteractive = gets $ isJust . shellAttrs

-----------------------------------------------------------------------------
-- Job control

-- | The shell ignores these signals becuase it cannot itself be job
-- controlled.
jobControlSignals :: [Signal]
jobControlSignals = 
  [ keyboardSignal, keyboardTermination, keyboardStop
  , backgroundRead, backgroundWrite
  ]

-- | Create a new process in the given job. Add it to the list of
-- processes controlled by the shell.
addProcess :: Job -> ProcessID -> Shell Process
addProcess job pid = modify (shellProcs_u $ Map.insert pid proc) >> return proc
  where
    proc = Process { processID     = pid
                   , processJob    = jobID job
                   , processStatus = Nothing
                   }

-- | Create a new job with a given command line. Do _not_ add to the
-- set of processes controlled by the shell
newJob :: String -> Shell Job
newJob cmdLine = do
  (jid:jids) <- gets shellJobIDs
  modify $ shellJobIDs_s jids
  return $ Job { jobID      = jid
               , jobProcs   = []
               , jobPgid    = 0
               , jobAttrs   = Nothing
               , jobCmdLine = cmdLine
               , jobStatus  = JobRunning
               }

-- | Execute each of the actions in the input list in a process of its
-- own, piping their input and outputs together.
startJob :: String -> Bool -> [IO ()] -> Shell ()
startJob cmdLine foreground procs = do
  inFd <- gets shellInFd
  job <- newJob cmdLine >>= start'em inFd procs
  modify $ shellJobs_u $ Map.insert (jobID job) job
  awaitJob False $ jobID job
  where
    start'em :: Fd -> [IO ()] -> Job -> Shell Job
    start'em _    []     job = return job
    start'em inFd (p:ps) job = do
      (nextIn, outFd) <- if null ps then liftM ((,) undefined) $ gets shellOutFd
                                    else liftIO $ createPipe

      -- Fork off the new process
      state <- get
      pid <- liftIO $ forkProcess $ evalShell state $ do
               put state
               startProcess p (jobPgid job) inFd outFd stdError foreground

      -- First process becomes process group id if interactive
      let job' = jobPgid_u (\pgid -> if pgid == 0 then pid else pgid) job
          handler :: SomeException -> IO ()
          handler = const $ return ()
      whenM isInteractive $ liftIO $ setProcessGroupID pid (jobPgid job')
                                      `catch` handler
      tcsetpgrp (jobPgid job')

      -- Parent closes pipes
      when (inFd  /= stdInput)  $ liftIO $ closeFd inFd
      when (outFd /= stdOutput) $ liftIO $ closeFd outFd

      -- Start the rest of the processes
      addProcess job pid
      start'em nextIn ps (jobProcs_u ((pid, JobRunning):) job')


-- | Execute the io action in the child process
startProcess :: IO () -> ProcessGroupID -> Fd -> Fd -> Fd -> Bool -> Shell ()
startProcess proc pgid inFd outFd errFd foreground = do
  -- If the shell is non-interactive put the job in the new group
  whenM isInteractive $ do
    pid <- liftIO $ getProcessID

    -- The first process in the job sets the process group to its own pid
    let pgid' = if pgid == 0 then pid else pgid
    liftIO $ setProcessGroupID pid pgid'

    -- If we run in the foreground, take control of the terminal,
    -- this can possibly raise SIGTTOU so ignore that temporarily.
    when foreground $ do
      liftIO $ installHandler backgroundWrite Ignore Nothing    
      tcsetpgrp pgid'

    -- Restore the signals to default to enable job control.
    mapM_ (\sig -> liftIO $ installHandler sig Default Nothing)
          jobControlSignals
  -- end when Interactive 

  -- Setup the file descriptors
  let dupIt from to = when (from /= to) $
                        liftIO $ dupTo from to >> closeFd from
  zipWithM_ dupIt [inFd, outFd, errFd] [stdInput, stdOutput, stdError]

  -- Execute the io action and exit
  liftIO $ (proc >> hFlush stdout >> exitWith ExitSuccess)

backgroundJob :: JobID -> Shell ()
backgroundJob = withJob $ \job -> do
  let udProcs  = jobProcs_u $ map $ \(pid, _) -> (pid, JobRunning)
  let udStatus = jobStatus_s $ JobRunning
  modify $ shellJobs_u $ Map.insert (jobID job) $ udProcs $ udStatus job
  liftIO $ signalProcessGroup continueProcess $ jobPgid job


withJob :: (Job -> Shell ()) -> JobID -> Shell ()
withJob m jid = do
  gets shellJobs >>= maybe (liftIO $ putStrLn "Unknown job") m . Map.lookup jid
   


prettyJobs :: Shell Doc
prettyJobs = gets (vcat . map prettyJob . Map.elems . shellJobs)

prettyJob :: Job -> Doc
prettyJob job = jid <+> status <+> cmdLine
  where
    jid     = brackets (text $ show $ jobID job)
    status  = text $ drop 3 $ show $ jobStatus job
    cmdLine = text $ jobCmdLine job




-- | Check to see if any children have changed status. If the argument
-- is 'True' the function will block.
checkForChildren :: Bool -> Shell (Maybe Job)
checkForChildren block = do
  haveNoChildren <- (gets $ Map.null . shellJobs)
  if haveNoChildren then return Nothing else do
  maybeStatus <- liftIO $ getAnyProcessStatus block True
  case maybeStatus of
    Nothing               -> return Nothing
    Just ps@(pid, status) -> do
      let udl f a = first fromJust . Map.updateLookupWithKey (\_ -> Just . f) a
      -- Find the process which changed and update its status
      procs <- gets shellProcs
      let (proc, procs') = udl (processStatus_s $ Just status) pid procs

      -- Find the Job of which the process is part and update the status
      jobs <- gets shellJobs
      let (job, jobs') = udl (updateJobStatus ps) (processJob proc) jobs

      -- Remember what we have done and return
      modify $ shellJobs_s jobs' . shellProcs_s procs'
      return $ Just job


-- | Update the status of a job when a process changes its status
updateJobStatus :: (ProcessID, ProcessStatus) -> Job -> Job
updateJobStatus (pid, pStatus) job = udJob $ jobProcs_u (map udProc) job
  where
    status = processToJobStatus pStatus
    udProc (pid', status') = (pid', if pid == pid' then status else status')
    udJob job'
      | any ((==JobStopped) . snd) $ jobProcs job' = jobStatus_s JobStopped job'
      | all ((==status) . snd) $ jobProcs job'     = jobStatus_s status job'
      | otherwise                                  = job'

processToJobStatus :: ProcessStatus -> JobStatus
processToJobStatus ps = case ps of
  Exited _     -> JobDone
  Terminated _ -> JobTerminated
  Stopped _    -> JobStopped


awaitJob :: Bool -> JobID -> Shell ()
awaitJob restart = withJob $ \job -> do
  whenM isInteractive $ tcsetpgrp (jobPgid job)
  -- Set the terminal attributes for the stopped job.
  when restart $ do
    tcsetattr $ fromJust $ jobAttrs job
    liftIO $ signalProcessGroup continueProcess $ jobPgid job
  _ <- loop $ jobID job
  -- Restore terminal
  whenM isInteractive $ do
    gets (fromJust . shellAttrs) >>= tcsetattr
    gets shellPgid >>= tcsetpgrp
  where
    loop :: JobID -> Shell JobStatus
    loop jid = do
      mJob <- checkForChildren True
      case mJob of
        Just job | jobID job == jid ->
          case jobStatus job of
            JobStopped    -> stopJob job
            JobTerminated -> removeJob job
            JobDone       -> removeJob job
            _             -> loop jid
        _ -> loop jid


setJob :: Job -> Shell ()
setJob job = modify $ shellJobs_u $ Map.insert (jobID job) job

stopJob :: Job -> Shell JobStatus
stopJob job = do
  tcgetattr >>= setJob . flip jobAttrs_s job . Just
  return $ jobStatus job

removeJob :: Job -> Shell JobStatus
removeJob job = do
  modify $ shellJobIDs_u $ insert $ jobID job
  modify $ shellJobs_u $ Map.delete (jobID job)
  modify $ shellProcs_u $ flip (foldr (Map.delete . fst)) $ jobProcs job
  return $ jobStatus job

-----------------------------------------------------------------------------
-- | Functions to control the terminal associated with the shell

tcgetpgrp :: Shell ProcessGroupID
tcgetpgrp = gets shellInFd >>= liftIO . getTerminalProcessGroupID

tcsetpgrp :: ProcessGroupID -> Shell ()
tcsetpgrp pgid = do
  fd <- gets shellInFd
  liftIO $ setTerminalProcessGroupID fd pgid

tcgetattr :: Shell TerminalAttributes
tcgetattr = gets shellInFd >>= liftIO . getTerminalAttributes

tcsetattr :: TerminalAttributes -> Shell ()
tcsetattr attrs = do
  fd <- gets shellInFd
  liftIO $ setTerminalAttributes fd attrs WhenDrained

isatty :: Shell Bool
isatty = gets shellInFd >>= liftIO . liftM (0 /=) . c_isatty

foreign import ccall unsafe "unistd.h isatty" c_isatty :: Fd -> IO CInt

-- Generated by DrIFT
shellAttrs_u :: (Maybe TerminalAttributes -> Maybe TerminalAttributes) -> ShellState -> ShellState
shellAttrs_u f r@ShellState{shellAttrs  = x} = r{shellAttrs = f x}

shellJobIDs_u :: ([JobID] -> [JobID]) -> ShellState -> ShellState
shellJobIDs_u f r@ShellState{shellJobIDs  = x} = r{shellJobIDs = f x}

shellJobs_u :: (Map JobID Job -> Map JobID Job) -> ShellState -> ShellState
shellJobs_u f r@ShellState{shellJobs  = x} = r{shellJobs = f x}

shellPgid_u :: (ProcessID -> ProcessID) -> ShellState -> ShellState
shellPgid_u f r@ShellState{shellPgid  = x} = r{shellPgid = f x}

shellProcs_u :: (Map ProcessID Process -> Map ProcessID Process) -> ShellState -> ShellState
shellProcs_u f r@ShellState{shellProcs  = x} = r{shellProcs = f x}

shellAttrs_s :: Maybe TerminalAttributes -> ShellState -> ShellState
shellAttrs_s v =  shellAttrs_u  (const v)

shellJobIDs_s :: [JobID] -> ShellState -> ShellState
shellJobIDs_s v =  shellJobIDs_u  (const v)

shellJobs_s :: Map JobID Job -> ShellState -> ShellState
shellJobs_s v =  shellJobs_u  (const v)

shellPgid_s :: ProcessID -> ShellState -> ShellState
shellPgid_s v =  shellPgid_u  (const v)

shellProcs_s :: Map ProcessID Process -> ShellState -> ShellState
shellProcs_s v =  shellProcs_u  (const v)

processStatus_u :: (Maybe ProcessStatus -> Maybe ProcessStatus) -> Process -> Process
processStatus_u f r@Process{processStatus  = x} = r{processStatus = f x}

processStatus_s :: Maybe ProcessStatus -> Process -> Process
processStatus_s v =  processStatus_u  (const v)

jobAttrs_u :: (Maybe TerminalAttributes -> Maybe TerminalAttributes) -> Job -> Job
jobAttrs_u f r@Job{jobAttrs  = x} = r{jobAttrs = f x}

jobPgid_u :: (ProcessGroupID -> ProcessGroupID) -> Job -> Job
jobPgid_u f r@Job{jobPgid  = x} = r{jobPgid = f x}

jobProcs_u :: ([(ProcessID,JobStatus)] -> [(ProcessID,JobStatus)]) -> Job -> Job
jobProcs_u f r@Job{jobProcs  = x} = r{jobProcs = f x}

jobStatus_u :: (JobStatus -> JobStatus) -> Job -> Job
jobStatus_u f r@Job{jobStatus  = x} = r{jobStatus = f x}

jobAttrs_s :: Maybe TerminalAttributes -> Job -> Job
jobAttrs_s v =  jobAttrs_u  (const v)

jobStatus_s :: JobStatus -> Job -> Job
jobStatus_s v =  jobStatus_u  (const v)
