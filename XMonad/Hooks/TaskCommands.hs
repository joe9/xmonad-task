
{-# OPTIONS_GHC -Wall #-}
module XMonad.Hooks.TaskCommands
   ( serverModeTaskEventHookCmd'
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

-- $usage
--
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- >    import XMonad.Hooks.ServerMode
-- >    import XMonad.Actions.BluetileActions
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
serverModeTaskEventHookCmd' :: TaskActions a b
                               -> X [(String,X ())] -> Event -> X All
serverModeTaskEventHookCmd' taskcommands cmdAction =
  serverModeEventHookF "XMONAD_COMMAND"
                    (sequence_ . commandHelper taskcommands cmdAction)

-- got the below code from XMonad.Hooks.ServerMode
commandHelper :: TaskActions a b -> X [(String,X ())] -> String -> [X()]
commandHelper taskcommands cmdAction cmdl
  | "TaskAction " `isPrefixOf` cmdl =
      [ addWorkspaceForTask cmdl
        . readMay
        . drop (length "TaskAction ")
        $ cmdl
      , currentWorkspaceAction taskcommands
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
