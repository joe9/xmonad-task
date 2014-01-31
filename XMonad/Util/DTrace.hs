
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module XMonad.Util.DTrace
   ( dtrace
   , groom
   ) where

-- system imports
import           Data.Time  (getCurrentTime)
import           Text.Groom (groom)

-- xmonad core
import           XMonad     (trace)

dtrace :: Show a => a -> IO ()
dtrace s = getCurrentTime
           >>= (\dt -> trace $ unwords [show dt, " xmonad:", groom s])
