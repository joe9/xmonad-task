
{-# OPTIONS_GHC -Wall #-}
module XMonad.Config.TaskActionConfig
   ( taskActions
   , terminals
   , toLayout
   , xmobarShowTaskDirNumTitle
   , gridSelectShowWorkspace
   ) where

-- system imports
import qualified Data.Map                         as M
import           Safe
import           System.FilePath
-- import           System.Process

-- xmonad core
import           XMonad                           hiding (focus,
                                                   workspaces)
import           XMonad.StackSet


-- xmonad contrib
-- import           XMonad.Actions.SpawnOn
import           XMonad.Hooks.DynamicLog
import           XMonad.Layout.IndependentScreens
import           XMonad.Layout.LayoutCombinators

import           XMonad.Actions.Task

taskActions :: TaskActions (Int,String) (Int,String)
taskActions =
  M.fromList
    [ ("terminal"  , taf (terminals 1))
    , ("1terminal" , taf (terminals 1))
    , ("2terminal" , ta (terminals 2))
    , ("3terminal" , ta (terminals 3))
    , ("None"      , ta (\_ -> return ()))
    ]
  where ta f  = nullTaskAction
                 { taStartup    = f
                 , taXmobarShow = xmobarShowTaskDirNumTitle
                 , taGridSelectShow = gridSelectShowWorkspace
                 }
        taf f = nullTaskAction
                 { taStartup = (\t -> f t >> l "Full")
                 , taXmobarShow = xmobarShowTaskDirNumTitle
                 , taGridSelectShow = gridSelectShowWorkspace
                 }
      --tam f = nullTaskAction {taStartup = (\t -> f t >> l "Mosaic")}
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

