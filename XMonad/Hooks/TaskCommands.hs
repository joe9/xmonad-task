
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module XMonad.Hooks.TaskCommands
   ( taskCommands
   , serverModeTaskEventHookCmd'
   , terminals
   ) where

-- system imports
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Safe
import           System.IO

-- xmonad core
import           XMonad

-- xmonad contrib
import           XMonad.Hooks.ServerMode

import           XMonad.Actions.Task

taskCommands :: TaskCommands
taskCommands = myTaskCommands

myTaskCommands :: TaskCommands
-- myTaskCommands = Data.Map.fromList
--                   [ ("Nothing"   , (\_ -> liftIO (putStrLn "nothing")
--                                           >> return ()))
--                   ]
myTaskCommands = [ ("terminal" , terminals 1)
                  , ("1terminal" , terminals 1)
                  , ("2terminal" , terminals 2)
                  -- , ("Nothing"   , (\_ -> return ()))
                  , ("Nothing"   , (\_ -> return ()))
                  ]

terminals :: Int -> Task -> X()
terminals n t = (spawnShellIn . tDir $ t) >*> n

-- $usage
--
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- >    import XMonad.Hooks.ServerMode
-- >    import XMonad.Actions.BluetileCommands
--
-- Then edit your @handleEventHook@:
--
-- > main = xmonad def { handleEventHook = serverModeEventHook' bluetileCommands }
--
-- See the documentation of "XMonad.Hooks.ServerMode" for details on
-- how to actually invoke the commands from external programs.


-- | Executes a command of the list when receiving its name via a special ClientMessageEvent.
-- Uses "XMonad.Actions.Commands#defaultCommands" as the default.
--
-- > main = xmonad def { handleEventHook = serverModeEventHookCmd }
--
-- > xmonadctl run # Tells xmonad to generate a run prompt
--
-- serverModeEventHookCmd :: Event -> X All
-- serverModeEventHookCmd = serverModeEventHookCmd' defaultCommands

-- run the command ./XMonad/Util/taskctl -- "TaskCommand Task {tCommand = \"terminal\", tScreen = S 1, tDir = \"/home/j/\", tLayout = Just \"Full\", tId = 0}"
--                 to create a task

-- | Additionally takes an action to generate the list of commands
serverModeTaskEventHookCmd' :: [(String,Task -> X ())]
                               -> X [(String,X ())] -> Event -> X All
serverModeTaskEventHookCmd' taskcommands cmdAction ev =
  serverModeEventHookF "XMONAD_COMMAND"
                    (sequence_ . commandHelper taskcommands cmdAction)
                    ev

-- got the below code from XMonad.Hooks.ServerMode
commandHelper :: [(String,Task -> X ())]
                 -> X [(String,X ())] -> String -> [X()]
commandHelper taskcommands cmdAction cmdl
  | isPrefixOf ("TaskCommand ") cmdl =
      [ addWorkspaceForTask cmdl
        . readMay
        . drop (length "TaskCommand ")
        $ cmdl
      , currentWorkspaceCommand taskcommands
      ]
  | otherwise =
        map helper . words $ cmdl
        where helper cmd =
                do cl <- cmdAction
                   trace "command received: "
                   trace cmd
                   fromMaybe (io $ hPutStrLn
                                    stderr
                                    ("Couldn't find command " ++ cmd))
                             (lookup cmd cl)
