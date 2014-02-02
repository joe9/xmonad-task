
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module XMonad.Config.XmobarConfig
   ( barCreator
   , barDestroyer
   , xmobarCommand
   , pp
   ) where

-- system imports
import           Data.Default                     (Default (def))

-- xmonad core
import           XMonad                           (ScreenId (S),
                                                   WindowSpace, X,
                                                   withWindowSet)
import           XMonad.StackSet                  (Screen (screen),
                                                   StackSet (current))

-- xmonad contrib
import           XMonad.Hooks.DynamicBars         (DynamicStatusBar, DynamicStatusBarCleanup)
import           XMonad.Hooks.DynamicLog          (PP (ppCurrent, ppHiddenNoWindows, ppOrder, ppSep, ppSort, ppTitle, ppUrgent, ppVisible),
                                                   shorten,
                                                   xmobarColor)
import           XMonad.Layout.IndependentScreens (marshallSort)
import           XMonad.Util.Run                  (spawnPipe)

--local, for debugging, similar to IPPrint
--import PPrint
-- import           XMonad.Actions.Task
import           XMonad.Util.DTrace               (dtrace)

barCreator :: DynamicStatusBar
barCreator (S sid) = do dtrace ("CREATING xmobar " ++ show sid)
                        spawnPipe . xmobarCommand $ sid

xmobarCommand :: Int -> String
xmobarCommand s =
   unwords [ "xmobar"
           , cfg s
           , scrn s
           -- , template s
           ]
   where -- template 0 = ""
         -- template _ = "--template=%StdinReader%"
         configLocation = "/home/j/etc/xmobar/"
         cfg 0 = configLocation ++ "xmobarrc.acer2223w_monitor"
         cfg _ = configLocation ++ "xmobarrc.acer1912_monitor"
         scrn 0 = "--screen=0"
         scrn _ = "--screen=1"

barDestroyer :: DynamicStatusBarCleanup
-- barDestroyer = return ()
barDestroyer = dtrace "DESTROYING xmobar"

pp :: PP
pp =
  def {
      ppCurrent           = xmobarColor "white" ""
    , ppVisible           = xmobarColor "white" ""
    , ppHiddenNoWindows   = const ""
    , ppUrgent            = xmobarColor "red" "yellow"
    , ppSep               = " : "
    , ppOrder             =
      \(wss:layoutString:titleString:_)
         -> [ addPosition . words $ wss
            -- , removeMinimize layoutString
            , layoutString
            , titleString ]
    , ppTitle             = xmobarColor "green" "" . shorten 126
    -- ppSort              = return id
    , ppSort              = showOnlyCurrentScreenWorkspaces
    }
--   where removeMinimize =
--           ( unwords
--             . tailNote "XmobarConfig.hs pp: formatting layoutstring"
--             . words
--           )
        -- where ssp = stripScreenPrefix
        -- below from XMonad.Layout.IndependentScreens

addPosition :: [String] -> String
addPosition ([])    =  []
addPosition (x:[])  =  x
addPosition (x:xs) =
    unwords
    . (:) x
    -- change the below [0..9] to a longer symbol stream
    . zipWith (\a b -> show a ++ ":" ++ b)
              [0 .. 9 :: Int]
    $ xs
    -- where f = xmobarColor "yellow" ""

-- below from XMonad.Layout.IndependentScreens
showOnlyCurrentScreenWorkspaces :: X ([WindowSpace] -> [WindowSpace])
showOnlyCurrentScreenWorkspaces =
   withWindowSet
      $ \ws -> return $ flip marshallSort id . screen . current $ ws

--    gets (screen . current . windowset)
--      >>= (\s -> return $ marshallSort s id)

-- stripScreenPrefix :: String -> String
-- stripScreenPrefix ('0':'_':xs) = xs
-- stripScreenPrefix ('1':'_':xs) = xs
-- stripScreenPrefix x = x

-- got the below idea from the function getXineramaWsCompare'
--      of XMonad-Util-WorkspaceCompare.html
-- showOnlyCurrentScreenWorkspaces :: X ([WindowSpace] -> [WindowSpace])
-- showOnlyCurrentScreenWorkspaces = do
--     w <- gets windowset
--     let (S sid) = S.screen . S.current $ w
--         -- ws' = S.filter ((==) sid . S.screen) (S.current w ++ S.visible w)
--         ws' = (map S.workspace
--                 . Prelude.filter ((==) (S sid) . S.screen)
--                 $ (S.current w : S.visible w))
--               -- ++ L.filter (L.isPrefixOf (show sid) . show . S.tag) (S.hidden w)
--               -- ++ L.filter (\x -> True) (S.hidden w)
--               ++ L.filter (L.isPrefixOf (show sid) . S.tag) (S.hidden w)
--     -- trace $ "showing visible windows: " ++ concatMap show . S.visible $ w
--     -- trace . show . S.visible $ w
--     -- trace . show $ sid
--     -- trace . show . map S.tag $ ws'
--     -- return $ \x -> ws'
--     return $ const ws'

