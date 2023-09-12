{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

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


-- | Deprecated!
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
