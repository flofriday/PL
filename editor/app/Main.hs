module Main (main) where

import Lib

import Data.GI.Base
import qualified GI.Gtk as Gtk

main :: IO ()
main = do
  _ <- Gtk.init Nothing
  return ()
