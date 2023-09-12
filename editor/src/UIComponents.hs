{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module UIComponents(
  MenuDescription,
  createMenuBar,
  menuBarDescr,
  createEditorView
) where
import qualified GI.Gtk as Gtk

import Control.Monad (void)
import Data.Text (Text)
import GI.Gtk (MenuBar, MenuItem, Menu, menuBarNew, menuItemNewWithLabel, menuItemNewWithMnemonic, menuItemSetSubmenu, menuShellAppend, menuNew, onMenuItemActivate, mainQuit)
import qualified GI.Gtk as Gtk
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.GI.Base

import MenuFileActions (openFileDialog, saveFileDialog)


-- TODO: Add UI Components
-- haskell-gi examples can be found here https://github.com/haskell-gi/gi-gtk-examples/tree/master


type MenuDescription = [(Text, [(Text, Maybe (IO ()))])]
menuBarDescr :: MenuDescription
menuBarDescr =
    [ ("_File", [ ("Open", Just openFileDialog)
                , ("Save", Just saveFileDialog)
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


createEditorView :: Gtk.TextBuffer -> IO Gtk.ScrolledWindow
createEditorView txtBuffer = do
    scrollWindow <- Gtk.scrolledWindowNew (Nothing :: Maybe Gtk.Adjustment) (Nothing :: Maybe Gtk.Adjustment)
    textView <- Gtk.new Gtk.TextView [#buffer := txtBuffer, #leftMargin := 10, #topMargin := 10 ]

    -- add text view styling
    styleProvider <- Gtk.cssProviderNew
    Gtk.cssProviderLoadFromData styleProvider
        "textview text { \
        \    caret-color: #FFFFFF; \
        \    background-color: #1E1F22; \
        \    color: #BCBEC4; \
        \} \
        \ textview { \
        \    font-size: 16px; \
        \}"
    screen <- Gtk.widgetGetScreen textView
    Gtk.styleContextAddProviderForScreen screen styleProvider 800

    Gtk.widgetShowAll textView -- must show before add notebook,
                           -- otherwise notebook won't display child widget
                           -- even have add in notebook.

    Gtk.containerAdd scrollWindow textView
    Gtk.widgetShowAll scrollWindow
    return scrollWindow