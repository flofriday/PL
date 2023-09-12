{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module UIComponents(
  MenuDescription,
  createMenuBar,
) where

import Control.Monad (void)
import Data.Text (Text)
import GI.Gtk (MenuBar, MenuItem, Menu, menuBarNew, menuItemNewWithLabel, menuItemNewWithMnemonic, menuItemSetSubmenu, menuShellAppend, menuNew, onMenuItemActivate)
import qualified Data.Text as T


import qualified FileOperations

-- TODO: Add UI Components
-- haskell-gi examples can be found here https://github.com/haskell-gi/gi-gtk-examples/tree/master

type MenuDescription = [(Text, [(Text, Maybe (IO ()))])]

createMenuBar :: MenuDescription -> IO MenuBar
createMenuBar descr = do
  bar <- menuBarNew
  mapM_ (createMenu bar) descr
  return bar
  where
    createMenu :: MenuBar -> (Text, [(Text, Maybe (IO ()))]) -> IO ()
    createMenu bar (name, items) = do
      menu <- menuNew
      item <- menuItemNewWithLabelOrMnemonic name
      menuItemSetSubmenu item (Just menu)
      menuShellAppend bar item
      mapM_ (createMenuItem menu) items

    createMenuItem :: Menu -> (Text, Maybe (IO ())) -> IO ()
    createMenuItem menu (name, action) = do
      item <- menuItemNewWithLabelOrMnemonic name
      menuShellAppend menu item
      case action of
        Just act -> void $ onMenuItemActivate item act
        Nothing  -> void $ onMenuItemActivate item (return ())

    menuItemNewWithLabelOrMnemonic :: Text -> IO MenuItem
    menuItemNewWithLabelOrMnemonic name
      | '_' `elem` T.unpack name = menuItemNewWithMnemonic name
      | otherwise                = menuItemNewWithLabel name