
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module XMonad.Util.DTrace
   ( dtrace
   , groom
   ) where

-- system imports
import           Data.Time
import           Text.Groom

-- xmonad core
import           XMonad

dtrace :: Show a => a -> IO ()
dtrace s = getCurrentTime
           >>= (\dt -> trace $ unwords [show dt, " xmonad:", groom s])
