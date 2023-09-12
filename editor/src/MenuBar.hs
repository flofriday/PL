{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MenuBar(
  MenuDescription,
  createMenuBar,
  menuBarDescr,
) where

import Control.Monad (void)
import Data.Text (Text)
import GI.Gtk (MenuBar, MenuItem, Menu, menuBarNew, menuItemNewWithLabel, menuItemNewWithMnemonic, menuItemSetSubmenu, menuShellAppend, menuNew, onMenuItemActivate, mainQuit)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk

import MenuFileActions (openFileDialog, saveFileDialog)

type MenuDescription = [(Text, [(Text, Maybe (IO ()))])]
menuBarDescr :: Gtk.Notebook -> MenuDescription
menuBarDescr notebook =
    [ ("_File", [ ("Open (Ctrl + O)", Just (openFileDialog notebook))
                , ("Save (Ctrl + S)", Just (saveFileDialog notebook))
                , ("New Tab (Ctrl + N)", Nothing)
                , ("_Quit", Just mainQuit)
                ]
      )
    , ("Help", [ ("_Help", Nothing)
               ]
      )
    ]

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
