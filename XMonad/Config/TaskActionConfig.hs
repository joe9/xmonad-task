
{-# OPTIONS_GHC -Wall #-}
module XMonad.Config.TaskActionConfig
   ( WindowTitle
   , NumberOfWindows
   , taskActions
   , terminals
   , toLayout
   , xmobarShowTerminalDirNumTitle
   , xmobarShowTaskDirNumTitle
   , gridSelectShowWorkspace
   , gsNumActionTitle
   , spawnShellWithCommand
   , terminalWithCommand
   ) where

-- system imports
import           Data.List
import qualified Data.Map                         as M (fromList)
import           Safe                             (atDef, headDef,
                                                   readMay)
import           System.FilePath                  (dropTrailingPathSeparator,
                                                   equalFilePath,
                                                   isValid,
                                                   takeBaseName,
                                                   takeDirectory)
-- import           System.Process

-- xmonad core
import           XMonad                           hiding (focus,
                                                   workspaces)
import           XMonad.StackSet


-- xmonad contrib
import           XMonad.Actions.SpawnOn           (spawnHere)
import           XMonad.Hooks.DynamicLog          (xmobarColor)
import           XMonad.Layout.IndependentScreens (PhysicalWorkspace,
                                                   VirtualWorkspace,
                                                   marshall,
                                                   unmarshallS,
                                                   unmarshallW)
import           XMonad.Layout.LayoutCombinators  (JumpToLayout (JumpToLayout))
-- import           XMonad.Actions.SpawnOn

import           XMonad.Actions.Task
import           XMonad.Util.DTrace

type WindowTitle = String
type NumberOfWindows = Int
taskActions :: TaskActions (NumberOfWindows,WindowTitle)
                          (NumberOfWindows,WindowTitle)
taskActions =
  M.fromList
    [ ("terminal"  , taf $ terminals 1)
    , ("1terminal" , taf $ terminals 1)
    , ("2terminal" , ta  $ terminals 2)
    , ("3terminal" , ta  $ terminals 3)
    , ("None"      , ta  $ \_ -> return ())
    , ("mpv"   , tam $ terminals 1)
    , ("xmonad-compile"
        , tam (\t -> tct t "tail -f /var/log/xinit.log"
                        >> terminals 1 t
                        >> tct t "ghciw xmonad.hs"))
    ]
  where ta f  = nullTaskAction
                 { taStartup    = f
                 , taXmobarShow = xmobarShowTerminalDirNumTitle
                 , taGridSelectShow = gridSelectShowWorkspace
                 }
        taf f = nullTaskAction
                 { taStartup = f -- \t -> f t >> l "Full"
                 , taXmobarShow = xmobarShowTerminalDirNumTitle
                 , taGridSelectShow = gridSelectShowWorkspace
                 }
        tam f = TaskAction (\t -> f t >> l "Mosaic")
                            xActionNum
                            gsNumActionTitle
        tct   = flip terminalWithCommand
        l = toLayout

toLayout :: String -> X ()
toLayout = sendMessage . JumpToLayout

gridSelectShowWorkspace :: (NumberOfWindows,WindowTitle) -> Task -> WindowSpace -> String
gridSelectShowWorkspace (n,ttle) _ w =
  show n
    ++ " "
    ++ (marshall scr . xmobarShowTask . tag $ w)
    ++ " "
    -- ++ (headDef "" . words) t
    ++ ttle
  where scr = unmarshallS . tag $ w

terminals :: Int -> Task -> X()
terminals n t =
  (io . trace $ "starting in " ++ tDir t)
  -- (io . trace $ "mkdir --parents " ++ tDir t)
     -- >> io (callProcess "/bin/mkdir" ["--parents", tDir t])
     >> ((spawnShellIn . tDir $ t) >*> n)

xmobarShowTerminalDirNumTitle ::
  (NumberOfWindows,WindowTitle) -> Task -> PhysicalWorkspace -> String
xmobarShowTerminalDirNumTitle (n,ttle) task wsid
  | equalFilePath "/home/j" (tDir task)
    && "" /= ttle
    && isPrefixOf "tmux" ttle
    && (2 < length ws)
    && isValid ((words ttle) !! 2)
--     = xmobarColor "red" "" (d . init . atDef "" (words ttle) $ 1)
    = xmobarColor "white" "" (d . init . atDef "" (words ttle) $ 1)
         ++ "-"
--          ++ (tDir task)
--          ++ "-"
         ++ show n
         ++ "-"
         ++ atDef (headDef "" ws) ws 2
  | equalFilePath "/home/j" (tDir task)
    && "" /= ttle
    && isPrefixOf "emacs" ttle
    && (2 < length ws)
    && isValid ((words ttle) !! 3)
--     = xmobarColor "yellow" "" (de . init . atDef "" (words ttle) $ 3)
    = xmobarColor "white" "" (de . init . atDef "" (words ttle) $ 3)
         ++ "-"
         ++ show n
         ++ "-"
         ++ atDef (headDef "" ws) ws 0
  | otherwise = xmobarShowTaskDirNumTitle (n,ttle) task wsid
    where d = takeBaseName . dropTrailingPathSeparator
          ws = words ttle
          de = takeBaseName . takeDirectory . dropTrailingPathSeparator

xmobarShowTaskDirNumTitle ::
  (NumberOfWindows,WindowTitle) -> Task -> PhysicalWorkspace -> String
xmobarShowTaskDirNumTitle (n,ttle) task _ =
  -- xmobarColor "white" "" (xmobarShowTask wsid)
  xmobarColor "white" "" (t task)
   ++ "-"
   ++ show n
   ++ "-"
   ++ (headDef "" . words) ttle
  where t = takeBaseName . dropTrailingPathSeparator . tDir

xmobarShowTask :: PhysicalWorkspace -> String
xmobarShowTask w =
  showTask w . (readMay :: String -> Maybe Task) . unmarshallW $ w

showTask :: VirtualWorkspace -> Maybe Task -> String
showTask w (Nothing) = w
showTask _ (Just t) =
  takeBaseName . dropTrailingPathSeparator . tDir $ t

-- terminals :: Int -> Task -> X()
-- terminals n t = (spawnShellIn . tDir $ t) >*> n

spawnShellWithCommand :: String -> X ()
spawnShellWithCommand cmd = withWindowSet $
  flip spawnShellWithCommandIn cmd
    . tDir
    . workspaceIdToTask
    . currentTag

type Dir = FilePath
spawnShellWithCommandIn :: Dir -> String -> X ()
spawnShellWithCommandIn dir cmd =
  asks (terminal . config)
    >>= (\t -> spawnHere (command t)
               >> (io . dtrace $ command t)
        )
  -- $ ["cd", dir, "&&", "ZSHSTARTUPCMD="++cmd, t]
  -- got the below idea not using startup variables
  -- http://www.zsh.org/mla/users/2005/msg00599.html
  where command t = unwords  [ "cd", dir, "&&", t, "new-session"
                             , show . unwords
                                   $ [ "exec /bin/zsh -is eval"
                                     , show cmd
                                     ]
                             ]

terminalWithCommand :: String -> Task -> X()
terminalWithCommand cmd t =
  (io . trace $ "starting in " ++ tDir t)
  -- (io . trace $ "mkdir --parents " ++ tDir t)
     -- >> (io $ callProcess "/bin/mkdir" ["--parents", tDir t])
     >> (flip spawnShellWithCommandIn cmd . tDir $ t)

xActionNum :: (Int, String) -> Task -> WorkspaceId -> String
xActionNum (n,_) task _ =
  xmobarColor "white" "" (tAction task)
   ++ "-"
   ++ show n

gsNumActionTitle :: (NumberOfWindows,WindowTitle) -> Task -> WindowSpace -> String
gsNumActionTitle (n,ttle) task w =
  show n
    ++ " "
    ++ (marshall scr . tAction $ task)
    ++ " "
    ++ ttle
  where scr = unmarshallS . tag $ w
