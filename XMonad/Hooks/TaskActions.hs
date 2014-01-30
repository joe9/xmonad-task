
{-# OPTIONS_GHC -Wall #-}
module XMonad.Hooks.TaskActions
   ( taskActions
   , serverModeTaskEventHookCmd'
   , terminals
   , toLayout
   ) where

-- system imports
import           Data.List
import qualified Data.Map                        as M
import           Data.Maybe
import           Data.Monoid
import           Safe
import           System.IO
import           System.Process

-- xmonad core
import           XMonad

-- xmonad contrib
-- import           XMonad.Actions.SpawnOn
import           XMonad.Hooks.ServerMode
import           XMonad.Layout.LayoutCombinators

import           XMonad.Actions.Task

taskActions :: TaskActions
taskActions =
  M.fromList
    [ ("terminal" , taf (terminals 1))
    , ("1terminal" , taf (terminals 1))
    , ("2terminal" , ta (terminals 2))
    , ("3terminal" , ta (terminals 3))
    , ("Nothing"   , ta (\_ -> return ()))
    ]
  where ta f  = nullTaskAction {taStartup = f}
        taf f = nullTaskAction {taStartup = (\t -> f t >> l "Full")}
      --tam f = nullTaskAction {taStartup = (\t -> f t >> l "Mosaic")}
        l = toLayout

toLayout :: String -> X ()
toLayout = sendMessage . JumpToLayout

terminals :: Int -> Task -> X()
terminals n t =
  (io . trace $ "mkdir --parents " ++ tDir t)
     >> io (callProcess "/bin/mkdir" ["--parents", tDir t])
     >> ((spawnShellIn . tDir $ t) >*> n)

-- terminals :: Int -> Task -> X()
-- terminals n t = (spawnShellIn . tDir $ t) >*> n

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
serverModeTaskEventHookCmd' :: TaskActions
                               -> X [(String,X ())] -> Event -> X All
serverModeTaskEventHookCmd' taskcommands cmdAction =
  serverModeEventHookF "XMONAD_COMMAND"
                    (sequence_ . commandHelper taskcommands cmdAction)

-- got the below code from XMonad.Hooks.ServerMode
commandHelper :: TaskActions -> X [(String,X ())] -> String -> [X()]
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
