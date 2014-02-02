
{-# OPTIONS_GHC -Wall #-}
module XMonad.Config.TaskActionConfig
   ( taskActions
   , terminals
   , toLayout
   , xmobarShowTaskDirNumTitle
   , gridSelectShowWorkspace
   , gsNumActionTitle
   , spawnShellWithCommand
   , terminalWithCommand
   ) where

-- system imports
import qualified Data.Map                         as M (fromList)
import           Safe                             (headDef, readMay)
import           System.FilePath                  (dropTrailingPathSeparator,
                                                   takeBaseName)
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

taskActions :: TaskActions (Int,String) (Int,String)
taskActions =
  M.fromList
    [ ("terminal"  , taf $ terminals 1)
    , ("1terminal" , taf $ terminals 1)
    , ("2terminal" , ta  $ terminals 2)
    , ("3terminal" , ta  $ terminals 3)
    , ("None"      , ta  $ \_ -> return ())
    , ("mplayer"   , tam $ terminals 1)
    , ("xmonad-compile"
        , tam (\t -> tct t "tail -f /var/log/xinit.log"
                        >> terminals 1 t
                        >> tct t "ghciw xmonad.hs"))
    ]
  where ta f  = nullTaskAction
                 { taStartup    = f
                 , taXmobarShow = xmobarShowTaskDirNumTitle
                 , taGridSelectShow = gridSelectShowWorkspace
                 }
        taf f = nullTaskAction
                 { taStartup = \t -> f t >> l "Full"
                 , taXmobarShow = xmobarShowTaskDirNumTitle
                 , taGridSelectShow = gridSelectShowWorkspace
                 }
        tam f = TaskAction (\t -> f t >> l "Mosaic")
                            xActionNum
                            gsNumActionTitle
        tct   = flip terminalWithCommand
        l = toLayout

toLayout :: String -> X ()
toLayout = sendMessage . JumpToLayout

gridSelectShowWorkspace :: (Int,String) -> Task -> WindowSpace -> String
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

xmobarShowTaskDirNumTitle ::
  (Int,String) -> Task -> PhysicalWorkspace -> String
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

gsNumActionTitle :: (Int,String) -> Task -> WindowSpace -> String
gsNumActionTitle (n,ttle) task w =
  show n
    ++ " "
    ++ (marshall scr . tAction $ task)
    ++ " "
    ++ ttle
  where scr = unmarshallS . tag $ w
