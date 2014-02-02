
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.Task
-- Copyright   :  (c)
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Joe <joe9mail@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Turns your workspaces into a more task oriented system.
-----------------------------------------------------------------------

module XMonad.Actions.Task
   (
  -- * Overview
  -- $overview

  -- * Usage
  -- $usage
     Task(..)
   , TaskActions
   , TaskAction(..)
   -- , goto
   , doTaskAction
   , currentTaskAction
   , nullTaskAction
   , currentWorkspaceAction
   , (>*>)
   , switchNthLastFocused
   , shiftNthLastFocused
   , xmessage
   , startupTaskWorkspaces
   , taskToWorkspace
   , workspaceIdToTask
   , tasksPP
   , addWorkspaceForTask
   , spawnShell
   , spawnShellIn
   ) where

-- system imports
import           Control.Monad                    (replicateM_, when)

import           Data.List                        (filter, isPrefixOf)
import qualified Data.Map                         as M (Map, lookup)
import           Data.Maybe                       (isJust, isNothing)
import           Safe                             (atDef, fromJustDef,
                                                   fromJustNote,
                                                   readMay, readNote)
import           System.IO                        (hClose, hPutStr,
                                                   hPutStrLn, stderr)

-- xmonad core
import           XMonad                           hiding (focus,
                                                   workspaces)
import           XMonad.StackSet

-- xmonad contrib
import           XMonad.Actions.DynamicWorkspaces (addHiddenWorkspace)
import           XMonad.Actions.OnScreen          (Focus (FocusTag),
                                                   onScreen)
import           XMonad.Actions.SpawnOn           (spawnHere)
import           XMonad.Hooks.DynamicLog          (PP (ppCurrent, ppHidden, ppHiddenNoWindows, ppUrgent, ppVisible))
import           XMonad.Layout.IndependentScreens (marshall,
                                                   unmarshallW)
import           XMonad.Util.Run                  (spawnPipe)

-- below not contributed to xmonad contrib yet
import           XMonad.Util.DTrace               (dtrace)

type Dir = FilePath
type Name = String

data Task = Task { tAction :: Name
                 , tScreen :: ScreenId
                 , tDir    :: Dir          -- starting directory
                 , tId     :: Int
                 }
                 deriving (Eq, Read, Show)

type TaskActions a b = M.Map Name (TaskAction a b)

data TaskAction a b =
  TaskAction  { taStartup         :: Task -> X ()
               , taXmobarShow     :: a -> Task -> WorkspaceId -> String
               , taGridSelectShow :: b -> Task -> WindowSpace -> String
               }

nullTaskAction :: TaskAction a b
nullTaskAction = TaskAction (\_ -> return ()) (\_ _ w -> w) (\_ _ -> tag)

tasksPP :: Show a => (WindowSet -> X (M.Map WorkspaceId a))
                    -> TaskActions a b
                    -> PP
                    -> X PP
tasksPP f tas pp = do
  wsInfo <- withWindowSet f
  let g = tasksPP' tas wsInfo
  -- io $ dtrace wsInfo
  return $
    pp { ppCurrent         = ppCurrent         pp . g
       , ppVisible         = ppVisible         pp . g
       , ppHidden          = ppHidden          pp . g
       , ppHiddenNoWindows = ppHiddenNoWindows pp . g
       , ppUrgent          = ppUrgent          pp . g
       }

tasksPP' :: TaskActions a b -> M.Map WorkspaceId a -> WorkspaceId -> String
tasksPP' tas wsInfo wsid =
             (\f -> h f (M.lookup wsid wsInfo) t wsid)
                    . taXmobarShow
                    . fromJustDef nullTaskAction
                    . flip M.lookup tas
                    . tAction
                    $ t
  where h _  (Nothing) _  = id
        h f (Just b) task = f b task
        t = workspaceIdToTask wsid

workspaceIdToTask :: WorkspaceId -> Task
workspaceIdToTask =
  (readNote "workspaceIdToTask: not a task" :: String -> Task)
   . unmarshallW

taskToWorkspaceId :: ScreenId -> Task -> Maybe WorkspaceId
taskToWorkspaceId s t
  |  isTaskOnScreen s t = Just . marshall s . show $ t
  |  otherwise          = Nothing

isTaskOnScreen :: ScreenId -> Task -> Bool
isTaskOnScreen s = (==) s . tScreen

-- -- | Switch to the given workspace.
currentWorkspaceAction :: TaskActions a b -> X ()
currentWorkspaceAction tas =
  withWindowSet $ \ws ->
    when
       (all ($ ws) [ isCurrentWorkspaceATask . currentTag
                   , isNothing . stack . workspace . current
                   ])
       $ io (dtrace "currentWorkspaceAction: starting")
           >> currentTaskAction tas

isCurrentWorkspaceATask :: WorkspaceId -> Bool
isCurrentWorkspaceATask =
  isJust . (readMay :: String -> Maybe Task) . unmarshallW

-- goto :: Task -> X ()
-- goto t = switchTaskSpace t >> doTaskAction t

-- assumes that the task is current
currentTaskAction :: TaskActions a b -> X ()
currentTaskAction tas =
   withWindowSet $ doTaskAction tas . workspaceIdToTask . currentTag

-- assumes that the task is current
doTaskAction :: TaskActions a b -> Task -> X ()
doTaskAction tas t = (\f -> f t)
                        . taStartup
                        . fromJustDef nullTaskAction
                        . M.lookup (tAction t)
                        $ tas

type NumberOfScreens = Int
startupTaskWorkspaces :: Int -> NumberOfScreens -> [Task] -> [WorkspaceId]
startupTaskWorkspaces startId i ts =
  map (fromJustNote "startupTaskWorkspaces: should not be here")
    . Data.List.filter isJust
    . zipWith ($) fs
    $ [startId..]
    where fs = [taskToWorkspace (S s) t | t <- ts
                                        , s <- [0..(i-1)]
                                        , S s == tScreen t]

taskToWorkspace :: ScreenId -> Task -> Int -> Maybe WorkspaceId
taskToWorkspace s t i = taskToWorkspaceId s (t {tId = i})

-- | An alias for @flip replicateM_@
(>*>) :: Monad m => m a -> Int -> m ()
(>*>) = flip replicateM_
infix >*>

currentScreen :: WindowSet -> ScreenId
currentScreen = screen . current

isOnCurrentScreen :: ScreenId -> WorkspaceId -> Bool
isOnCurrentScreen (S i) = isPrefixOf (show i ++ "_")

workspacesOnCurrentScreen :: ScreenId -> [WindowSpace] -> [WindowSpace]
workspacesOnCurrentScreen s =
  Data.List.filter (isOnCurrentScreen s . tag)

visiblesOnCurrentScreen :: WindowSet -> [WindowSpace]
visiblesOnCurrentScreen ws =
  map workspace
  . Prelude.filter ((==) (currentScreen ws) . screen)
  . visible
  $ ws

hiddensOnCurrentScreen :: WindowSet -> [WindowSpace]
hiddensOnCurrentScreen ws =
  workspacesOnCurrentScreen (currentScreen ws) . hidden $ ws

nonEmptyHddensOnCurrentScreen :: WindowSet -> [WindowSpace]
nonEmptyHddensOnCurrentScreen =
  Data.List.filter (isJust . stack) . hiddensOnCurrentScreen

onNthLastFocussed :: Int -> (WorkspaceId -> WindowSet -> WindowSet) -> X ()
onNthLastFocussed i f = withWindowSet $ \ws -> do
    let ws' = visiblesOnCurrentScreen ws
              ++ nonEmptyHddensOnCurrentScreen ws
    windows
      . f
      . atDef (tag . workspace . current $ ws) (map tag ws')
      $ i

-- | Switch to the Nth last focused task or failback to the
-- 'defaultTask'.
switchNthLastFocused :: Int -> X ()
switchNthLastFocused i = onNthLastFocussed i greedyView

shiftNthLastFocused :: Int -> X ()
shiftNthLastFocused i = onNthLastFocussed i shift

-- save :: String -> IO ()
-- save = writeFile "/tmp/xmonad_debug.txt"

-- | Display the given message using the @xmessage@ program.
xmessage :: String -> IO ()
xmessage s = do
  h <- spawnPipe "xmessage -file -"
  hPutStr h s
  hClose h

addWorkspaceForTask :: String -> Maybe Task -> X ()
addWorkspaceForTask rawt Nothing =
  io $ hPutStrLn stderr ("addWorkspaceForTask: Could not read string to task: " ++ rawt)
addWorkspaceForTask _ (Just t) = withWindowSet $ \ws -> do
  let tid     = getMaximumTaskId ws + 1
      newtask = t {tId = tid}
      wsid    = marshall (tScreen newtask) (show newtask)
      wid     = wsid
  trace $ show tid
  trace $ show newtask
  trace wsid
  -- return ()
  addHiddenWorkspace wsid
  windows $ onScreen (greedyView wid) (FocusTag wid) (tScreen newtask)

getMaximumTaskId :: WindowSet -> Int
getMaximumTaskId =
  maximum
    . map (tId . fromJustNote "getMaximumTaskId: shuld not happen")
    . Data.List.filter isJust
    . map (readMay . unmarshallW . tag)
    . workspaces

spawnShell :: X ()
spawnShell =
  withWindowSet $ spawnShellIn . tDir . workspaceIdToTask . currentTag

spawnShellIn :: Dir -> X ()
spawnShellIn dir =
  asks (terminal . config)
    >>= (\t -> spawnHere . unwords $ ["cd", dir, "&&", t])

-- $overview
-- This module allows to organize your workspaces on a precise task
-- basis.  So instead of having a workspace called `work' you can
-- setup one workspace per task.  Here we call these workspaces,
-- tasks. The great thing with tasks is that one can attach a
-- directory, starting layout and ScreenId that makes sense to each
-- particular task.  One can also attach an action which will be
-- triggered when switching to a topic that does not have any windows
-- in it.  So you can attach your mail client to the mail task, some
-- terminals in the right directory to the xmonad task... This package
-- also provides a nice way to display your tasks in an historical
-- way using a custom `pprWindowSet' function. You can also easily
-- switch to recent tasks using this history of last focused tasks.

-- $usage
-- Here is an example of configuration using TopicSpace:
--
-- > -- The list of all tasks/workspaces of your xmonad configuration.
-- > -- The order is important, new tasks must be inserted
-- > -- at the end of the list if you want hot-restarting
-- > -- to work.
-- > startupTasks, tasks :: [Task]
-- > startupTasks = tasks
-- > tasks =
-- >   (  concat
-- >         . replicate 3
-- >         . map (\s -> Task "terminal" (S s) "/home/j/" 0)
-- >         $ [0..1])
-- >     ++   concatMap f   [ "/home/j/etc/zsh"
-- >                       , "/home/j/etc/X11"
-- >                       , "/home/j/etc/emacs/emacs.d"
-- >                       ]
-- >     ++   map ff1   [ "/home/j/Desktop/"
-- >                         , "/home/j/etc/emacs/emacs.d"
-- >                         , "/home/j/"
-- >                         , "/home/j/"
-- >                         , "/var/log"
-- >                         ]
-- >     ++   map ff0 [ "/home/j/"
-- >                 , "/home/j/etc/xmonad"
-- >                 ]
-- >   where
-- >       f d = map (\s -> Task "terminal" (S s) d 0) [0..1]
-- >       ff1 d = Task "terminal" (S 1) d 0
-- >       ff0 d = Task "terminal" (S 0) d 0
-- >
-- > taskActions :: TaskActions (Int,String) (Int,String)
-- > taskActions =
-- >   M.fromList
-- >     [ ("terminal"  , taf (terminals 1))
-- >     , ("1terminal" , taf (terminals 1))
-- >     , ("2terminal" , ta (terminals 2))
-- >     , ("3terminal" , ta (terminals 3))
-- >     , ("None"      , ta (\_ -> return ()))
-- >     ]
-- >   where ta f  = nullTaskAction
-- >                  { taStartup    = f
-- >                  , taXmobarShow = xmobarShowTaskDirNumTitle
-- >                  , taGridSelectShow = gridSelectShowWorkspace
-- >                  }
-- >         taf f = nullTaskAction
-- >                  { taStartup = \t -> f t >> l "Full"
-- >                  , taXmobarShow = xmobarShowTaskDirNumTitle
-- >                  , taGridSelectShow = gridSelectShowWorkspace
-- >                  }
-- >         l = toLayout
