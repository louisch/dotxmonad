module OpenApp (startAll) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import System.Exit
import System.IO
import System.Process

-- Disjunction of the two forms of starting a process in Haskell.
data Command = Command String | WithArgs FilePath [String]
-- Separates programs into two categories:
-- The kind that keeps running until the user manually terminates it, i.e. user
-- applications such as chromium.
-- The kind that terminates quickly, i.e. gnu shell commands, such as ls.
-- This module will check for the first kind and will not start a Persistent
-- Program twice. It will not, however, check the second kind.
data Program = Persistent Command | Terminates Command

-- List of commands to run at startup
toStart :: [Program]
toStart = [Terminates (WithArgs "setxkbmap" ["-option",
                                             "ctrl:swap_lwin_lctl",
                                             "terminate:ctrl_alt_bksp"]),
           Persistent (Command "xmobar"),
           Persistent (Command "stalonetray"),
           Persistent (Command "dropboxd")]

isNotStarted :: Program -> IO Bool
isNotStarted program =
  case program of
   (Persistent cmd) -> do
     (exitCode, _, _) <- pidOfCommand cmd
     case exitCode of
      ExitSuccess -> return False
      ExitFailure _ -> return True
   (Terminates cmd) -> return True
-- Wrapper around the pidof linux command
pidOfCommand :: Command -> IO (ExitCode, String, String)
pidOfCommand cmd = readProcessWithExitCode pidOf [cmdname] ""
  where
    cmdname = case cmd of
      (Command name) -> name
      (WithArgs name _) -> name
    pidOf = "pidof"

removeStarted :: [Program] -> IO [Program]
removeStarted programs = filterM isNotStarted programs

getProcess :: Command -> CreateProcess
getProcess cmd = case cmd of
  Command cmdname -> shell cmdname
  WithArgs cmdname args -> proc cmdname args

startProgram :: Program ->
                IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
startProgram program = createProcess (getProcess cmd)
  where
    cmd = case program of
      Persistent cmd -> cmd
      Terminates cmd -> cmd

startAll :: IO ()
startAll = do
  programsToStart <- removeStarted toStart
  mapM startProgram programsToStart
  return ()
