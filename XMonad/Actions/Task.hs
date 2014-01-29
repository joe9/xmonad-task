
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module XMonad.Actions.Task
   ( Task(..)
   -- , goto
   -- , tasks
   -- , allMyTaskNames
   -- , myTaskConfig
   -- , nonEmptyRecentsOnCurrentScreen
   -- , nonEmptyTags
   , TaskCommands
   -- , taskCommands
   , doTaskCommand
   , currentTaskCommand
   , currentWorkspaceCommand
   , (>*>)
   -- , terminals
   , switchNthLastFocused
   , shiftNthLastFocused
   , xmessage
   , startupTaskWorkspaces
   , taskToWorkspace
   , xmobarShowTask
   , sendLayoutMessage
   , xmobarShowTaskWithNumberOfWindows
   , tasksPP
   , numberOfWindows
   , showNumberOfWindows
   , addWorkspaceForTask
   , spawnShell
   , spawnShellIn
   , myTerminal
   , windowSpacesNumTitles
   ) where

-- system imports
import           Control.Monad

-- import           Data.Default
-- import           Data.List.Split
-- import qualified Data.Map
-- import           Data.Map                         hiding (map)
import           Data.List
import           Data.Maybe
import           Safe
import           System.FilePath
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
-- import           XMonad.Actions.CycleWindows
-- import           XMonad.Layout.LayoutCombinators
import           XMonad.Util.Run                  (spawnPipe)

-- below not contributed to xmonad contrib yet
-- import           XMonad.Util.DTrace

type Dir = FilePath
type Name = String

data Task = Task { tCommand :: Name
                 , tScreen  :: ScreenId
                 , tDir     :: Dir          -- starting directory
                 , tLayout  :: Maybe String -- starting layout
                 , tId      :: Int
                 }
                 deriving (Eq, Read, Show)

-- type TaskCommands = Data.Map.Map Name (Task -> X ())
type TaskCommands = [(String, Task -> X ())]

tasksPP :: PP -> X PP
tasksPP pp = do
  ws <- gets windowset
  -- wnts <- mapM windowNumTitles . workspaces $ ws
  wnts <- windowSpacesNumTitles ws
  -- let f = xmobarShowTaskWithNumberOfWindows ws
  let g = xmobarShowTaskWithNumberOfWindowsAndFocussedTitle wnts
  -- logTitle >>= trace . fromJustDef ""
  return $
    pp { ppCurrent         = ppCurrent         pp . g -- f
       , ppVisible         = ppVisible         pp . g -- f
       , ppHidden          = ppHidden          pp . g -- f
       , ppHiddenNoWindows = ppHiddenNoWindows pp . g -- f
       , ppUrgent          = ppUrgent          pp . g -- f
       }

xmobarShowTaskWithNumberOfWindowsAndFocussedTitle ::
  [(WorkspaceId,(Int,String))]
        -> PhysicalWorkspace
        -> String
xmobarShowTaskWithNumberOfWindowsAndFocussedTitle wnts wsid =
  xmobarColor "white" "" (xmobarShowTask wsid)
   ++ "-"
   ++ show n
   ++ "-"
   ++ (headDef "" . words) t
   where (n,t) = fromJustDef (0,"") $ Data.List.lookup wsid wnts

xmobarShowTaskWithNumberOfWindows :: WindowSet
                                     -> PhysicalWorkspace
                                     -> String
xmobarShowTaskWithNumberOfWindows ws wsid =
  xmobarColor "white" "" (xmobarShowTask wsid)
   ++ "-"
   ++ (numberOfWindowsInWorkspace ws wsid)

numberOfWindowsInWorkspace :: WindowSet -> WorkspaceId -> String
numberOfWindowsInWorkspace ws wsid =
  showNumberOfWindows . find ((==) wsid . tag) . workspaces $ ws

showNumberOfWindows :: Maybe WindowSpace -> String
showNumberOfWindows = show . numberOfWindows

numberOfWindows :: Maybe WindowSpace -> Int
numberOfWindows (Nothing) = 0
numberOfWindows (Just w) = numOfWindows w

numOfWindows :: WindowSpace -> Int
numOfWindows = length . integrate' . stack

windowSpacesNumTitles :: WindowSet -> X [(WorkspaceId,(Int,String))]
windowSpacesNumTitles = mapM windowSpaceNumTitle . workspaces

windowSpaceNumTitle :: WindowSpace -> X (WorkspaceId,(Int,String))
windowSpaceNumTitle ws = do
  wTitle <- maybe (return "") (fmap show . getName . focus) . stack $ ws
  return (tag ws,(numOfWindows ws, wTitle))

xmobarShowTask :: PhysicalWorkspace -> String
xmobarShowTask w =
  showTask w . (readMay :: String -> Maybe Task) . unmarshallW $ w

showTask :: VirtualWorkspace -> Maybe Task -> String
showTask w (Nothing) = w
showTask _ (Just t) =
  takeBaseName . dropTrailingPathSeparator . tDir $ t

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
isTaskOnScreen s = ((==) s) . tScreen

-- -- | Switch to the given workspace.
currentWorkspaceCommand :: TaskCommands -> X ()
currentWorkspaceCommand tas = do
  trace "currentWorkspaceCommand: started"
  wsid <- gets (tag . workspace . current . windowset)
  if (isCurrentWorkspaceATask wsid)
    then currentTaskCommand tas
    -- else return ()
    else io $ trace "currentWorkspaceCommand: isCurrentWorkspaceATask is false"

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
startupTaskWorkspaces :: NumberOfScreens -> [Task] -> [WorkspaceId]
startupTaskWorkspaces i ts =
  map (fromJustNote "startupTaskWorkspaces: should not be here")
    . Data.List.filter isJust
    . zipWith ($) fs
    $ [1..]
    where fs = [taskToWorkspace (S s) t | t <- ts
                                        , s <- [0..(i-1)]
                                        , (S s) == tScreen t]
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
isOnCurrentScreen (S i) = isPrefixOf ((show i) ++ "_")

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
switchNthLastFocused i = do
  onNthLastFocussed i greedyView

shiftNthLastFocused :: Int -> X ()
shiftNthLastFocused i = do
  onNthLastFocussed i shift

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
  let tid     = (getMaximumTaskId ws) + 1
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
spawnShellIn dir = spawnHere . unwords $ ["cd", dir, "&&", myTerminal]

-- below to run a command on startup
-- ZSHSTARTUPCMD="echo test" st -e tmux &
-- to add a session name or window name, use the below
-- tmux new-session -s "testing" -n "testing window"
myTerminal :: String
myTerminal = "/usr/bin/st -e /usr/bin/tmux"
-- myTerminal = "/usr/bin/urxvt"
