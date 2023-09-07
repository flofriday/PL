{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module FileOperations where

import Data.GI.Base
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import Data.Text (unpack, pack)
import System.IO (writeFile)

-- TODO: implement file operations