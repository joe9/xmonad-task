
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
   , TaskCommands
   -- , goto
   , doTaskCommand
   , currentTaskCommand
   , currentWorkspaceCommand
   , (>*>)
   , switchNthLastFocused
   , shiftNthLastFocused
   , xmessage
   , startupTaskWorkspaces
   , taskToWorkspace
   , workspaceIdToTask
   , sendLayoutMessage
   , tasksPP
   , addWorkspaceForTask
   , spawnShell
   , spawnShellIn
   , windowSpacesNumTitles
   ) where

-- system imports
import           Control.Monad

import           Data.List
import           Data.Maybe
import           Safe
import           System.IO

-- xmonad core
import           XMonad                           hiding (focus,
                                                   workspaces)
import           XMonad.StackSet

-- xmonad contrib
import           XMonad.Actions.DynamicWorkspaces
import           XMonad.Actions.OnScreen
import           XMonad.Actions.SpawnOn
import           XMonad.Hooks.DynamicLog
import           XMonad.Layout.IndependentScreens
import           XMonad.Layout.LayoutCombinators
import           XMonad.Util.NamedWindows
import           XMonad.Util.Run                  (spawnPipe)

-- below not contributed to xmonad contrib yet
import           XMonad.Util.DTrace

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
-- >         . map (\s -> Task "terminal" (S s) "/home/j/" Nothing 0)
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
-- >       f d = map (\s -> Task "terminal" (S s) d (Just "Full") 0) [0..1]
-- >       ff1 d = Task "terminal" (S 1) d (Just "Full") 0
-- >       ff0 d = Task "terminal" (S 0) d (Just "Full") 0
-- > taskCommands :: TaskCommands
-- > taskCommands = [ ("terminal" , terminals 1)
-- >                   , ("1terminal" , terminals 1)
-- >                   , ("2terminal" , terminals 2)
-- >                   , ("3terminal" , terminals 3)
-- >                   , ("Nothing"   , \_ -> return ())
-- >                   ]
--

type Dir = FilePath
type Name = String

data Task = Task { tCommand :: Name
                 , tScreen  :: ScreenId
                 , tDir     :: Dir          -- starting directory
                 , tLayout  :: Maybe String -- starting layout
                 , tId      :: Int
                 -- , tLabel   :: Maybe String
                 }
                 deriving (Eq, Read, Show)

-- type TaskCommands = Data.Map.Map Name (Task -> X ())
type TaskCommands = [(String, Task -> X ())]

tasksPP :: ([(WorkspaceId,(Int,String))] -> PhysicalWorkspace -> String)
           -> PP -> X PP
tasksPP f pp = do
  ws <- gets windowset
  -- wnts <- mapM windowNumTitles . workspaces $ ws
  wnts <- windowSpacesNumTitles ws
  -- let f = xmobarShowTaskWithNumberOfWindows ws
  -- let g = xmobarShowTaskWithNumberOfWindowsAndFocussedTitle wnts
  let g = f wnts
  -- logTitle >>= trace . fromJustDef ""
  return $
    pp { ppCurrent         = ppCurrent         pp . g -- f
       , ppVisible         = ppVisible         pp . g -- f
       , ppHidden          = ppHidden          pp . g -- f
       , ppHiddenNoWindows = ppHiddenNoWindows pp . g -- f
       , ppUrgent          = ppUrgent          pp . g -- f
       }

-- xmobarShowTaskWithNumberOfWindows :: WindowSet
--                                      -> PhysicalWorkspace
--                                      -> String
-- xmobarShowTaskWithNumberOfWindows ws wsid =
--   xmobarColor "white" "" (xmobarShowTask wsid)
--    ++ "-"
--    ++ (numberOfWindowsInWorkspace ws wsid)

-- numberOfWindowsInWorkspace :: WindowSet -> WorkspaceId -> String
-- numberOfWindowsInWorkspace ws wsid =
--   showNumberOfWindows . find ((==) wsid . tag) . workspaces $ ws

-- showNumberOfWindows :: Maybe WindowSpace -> String
-- showNumberOfWindows = show . numberOfWindows

-- numberOfWindows :: Maybe WindowSpace -> Int
-- numberOfWindows (Nothing) = 0
-- numberOfWindows (Just w) = numOfWindows w

numOfWindows :: WindowSpace -> Int
numOfWindows = length . integrate' . stack

windowSpacesNumTitles :: WindowSet -> X [(WorkspaceId,(Int,String))]
windowSpacesNumTitles = mapM windowSpaceNumTitle . workspaces

windowSpaceNumTitle :: WindowSpace -> X (WorkspaceId,(Int,String))
windowSpaceNumTitle ws = do
  wTitle <- maybe (return "") (fmap show . getName . focus) . stack $ ws
  return (tag ws,(numOfWindows ws, wTitle))

workspaceIdToTask :: WorkspaceId -> Task
workspaceIdToTask =
  (readNote "workspaceIdToTask: not a task" :: String -> Task)
   . unmarshallW

taskToWorkspaceId :: ScreenId -> Task -> Maybe WorkspaceId
-- taskToWorkspaceId :: ScreenId -> Task -> WorkspaceId
-- taskToWorkspaceId s t = marshall s . show $ t
taskToWorkspaceId s t
  |  isTaskOnScreen s t = Just . marshall s . show $ t
  |  otherwise          = Nothing

isTaskOnScreen :: ScreenId -> Task -> Bool
isTaskOnScreen s = (==) s . tScreen

-- -- | Switch to the given workspace.
currentWorkspaceCommand :: TaskCommands -> X ()
currentWorkspaceCommand tas = do
  io $ dtrace "currentWorkspaceCommand: started"
  wsid <- gets (tag . workspace . current . windowset)
  if isCurrentWorkspaceATask wsid
    then currentTaskCommand tas
    -- else return ()
    else io $ dtrace "currentWorkspaceCommand: isCurrentWorkspaceATask is false"

isCurrentWorkspaceATask :: WorkspaceId -> Bool
isCurrentWorkspaceATask =
  isJust . (readMay :: String -> Maybe Task) . unmarshallW

-- goto :: Task -> X ()
-- goto t = switchTaskSpace t >> doTaskCommand t

-- assumes that the task is current
currentTaskCommand :: TaskCommands -> X ()
currentTaskCommand tas =
   gets(workspaceIdToTask
                     . tag
                     . workspace
                     . current
                     . windowset)
   >>= doTaskCommand tas

-- assumes that the task is current
doTaskCommand :: TaskCommands -> Task -> X ()
doTaskCommand tas t = do
   workSpace <- gets(workspace . current . windowset)
   when (isNothing . stack $ workSpace)
     -- . (\f -> liftIO ( f t))
     . (\f -> f t)
     . fromJustDef (\_ -> return ())
     . lookup (tCommand t)
     $ tas
   when (isNothing . stack $ workSpace)
     $ useLayout t

-- allMyTaskNames :: [Name]
-- allMyTaskNames =
--    concatMap ((\x -> ["0_" ++ x, "1_" ++ x]) . tAction) tasks
-- allMyTaskNames = map taskName myTasks

useLayout :: Task -> X ()
useLayout = sendLayoutMessage . tLayout

sendLayoutMessage :: Maybe String -> X ()
sendLayoutMessage (Nothing) = return ()
sendLayoutMessage (Just l) = sendMessage . JumpToLayout $ l

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
-- startupTaskWorkspaces i ts =
--   map (fromJustNote "startupTaskWorkspaces: should not be here")
--     . Data.List.filter isJust
--     . zipWith ($) [taskToWorkspace (S s) t | t <- ts, s <- [0..(i-1)]]
--     $ [0..]

taskToWorkspace :: ScreenId -> Task -> Int -> Maybe WorkspaceId
taskToWorkspace s t i = taskToWorkspaceId s (t {tId = i})

-- | An alias for @flip replicateM_@
(>*>) :: Monad m => m a -> Int -> m ()
(>*>) = flip replicateM_
infix >*>

currentScreen :: WindowSet -> ScreenId
currentScreen = screen . current

isOnCurrentScreen :: ScreenId -> WorkspaceId
                             -> Bool
isOnCurrentScreen (S i) = isPrefixOf (show i ++ "_")

workspacesOnCurrentScreen :: ScreenId -> [WindowSpace]
                           -> [WindowSpace]
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

onNthLastFocussed :: Int -> (WorkspaceId -> WindowSet -> WindowSet)
                    -> X ()
onNthLastFocussed i f = do
    ws <- gets windowset
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
addWorkspaceForTask _ (Just t) = do
  ws <- gets windowset
  let tid     = getMaximumTaskId ws + 1
      newtask = t {tId = tid}
  trace $ show tid
  trace $ show newtask
  trace (marshall (tScreen newtask) (show newtask))
  -- return ()
  addHiddenWorkspace (marshall (tScreen newtask) (show newtask))
  windows $ onScreen (greedyView (marshall (tScreen newtask) (show newtask))) (FocusTag (marshall (tScreen newtask) (show newtask))) (tScreen newtask)
  useLayout newtask

getMaximumTaskId :: WindowSet -> Int
getMaximumTaskId =
  maximum
    . map (tId . fromJustNote "getMaximumTaskId: shuld not happen")
    . Data.List.filter isJust
    -- . map ((readMay :: String -> Maybe Task) . tag)
    . map (readMay . unmarshallW . tag)
    . workspaces

spawnShell :: X ()
spawnShell =
  gets (tag . workspace . current . windowset)
  >>= spawnShellIn . tDir . workspaceIdToTask

spawnShellIn :: Dir -> X ()
-- spawnShellIn dir = spawnHere . unwords $ ["cd", dir, "&&", myTerminal]
-- spawnShellIn dir = undefined
spawnShellIn dir =
  asks (terminal . config)
    >>= (\t -> spawnHere . unwords $ ["cd", dir, "&&", t])

