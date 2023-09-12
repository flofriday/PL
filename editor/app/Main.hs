{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Text (Text)
import Data.Monoid ((<>))
import GI.Gtk
       (containerRemove, IsContainer, boxReorderChild, widgetGetParent,
        IsWidget, IsBox, imageNewFromPixbuf, iconThemeLoadIcon,
        iconThemeGetDefault, Image, spinnerStop, widgetShow, spinnerStart,
        labelSetText, setWindowTitle, boxPackStart, toolButtonNew,
        spinnerNew, boxNew, mainQuit, onWidgetDestroy, containerAdd,
        notebookRemovePage, notebookPageNum, onToolButtonClicked,
        notebookAppendPageMenu, labelNew, widgetShowAll, textViewNew,
        onWidgetKeyPressEvent, windowSetPosition, windowSetDefaultSize,
        notebookNew, windowNew, ToolButton, Label, Spinner, Box
        ,setContainerChild, menuItemNewWithLabel, menuItemNewWithMnemonic,
        onMenuItemActivate, menuShellAppend, menuItemSetSubmenu, menuNew,
        menuBarNew
        )
import qualified Data.Text as T (unpack)
import qualified GI.Gtk as Gtk (main, init)
import GI.Gtk.Enums (Orientation(..), WindowType(..), WindowPosition(..))
import Data.GI.Base.Attributes (AttrOp(..), set)
import GI.Gdk (keyvalName, getEventKeyKeyval, getEventKeyState)
import GI.Gdk.Flags (ModifierType(..))
import GI.GLib (timeoutAdd, pattern PRIORITY_DEFAULT)
import GI.Gtk.Flags (IconLookupFlags(..))
import Control.Exception (catch)
import Data.GI.Base.BasicTypes (UnexpectedNullPointerReturn(..))

import qualified Highlighting
import Highlighting(HighlightCond(Keys, Expr))

import qualified Text.Read as TR
import Data.Maybe (isJust)

import Data.GI.Base
import qualified GI.Gtk as Gtk

import qualified Highlighting
import Highlighting(HighlightCond(Keys, Expr))

import Notebook
import UIComponents

menuBarDescr
    = [ ("_File", [ ("Open", Nothing)
                  , ("Save", Nothing)
                  , ("_Quit", Nothing)
                  ]
        )
      , ("Help",  [ ("_Help", Nothing)
                  ]
        )
      ]

-- HIGHLIGHTING
-- number recognitions lambda
isStringInt = isJust . (TR.readMaybe :: String -> Maybe Int)
-- highlight rules
rules =
      [ ("keywords", "blue", Keys ["lambda", "let", "defun"])
      , ("number_literals", "cyan", Expr isStringInt)
      , ("brackets", "purple", Keys ["(", ")"])
      , ("boolean", "green", Keys ["true", "false"])
      ]
-- word separators
separators = [' ', '(', ')', '\n']

-- | Main
main :: IO ()
main = do
  -- Init.
  Gtk.init Nothing

  -- Create window, notebook and menubar
  window <- windowNew WindowTypeToplevel
  notebook <- notebookNew
  menuBar <- createMenuBar menuBarDescr  

  -- Set window.
  windowSetDefaultSize window 800 600
  windowSetPosition window WindowPositionCenter
  setWindowTitle window "Hello World."

  -- Handle key press action.
  onWidgetKeyPressEvent window $ \e ->
    -- Create new tab when user press Ctrl+n
    getEventKeyState e >>= \case
      [ModifierTypeControlMask] ->
        getEventKeyKeyval e >>= keyvalName >>= \case
          Just "n" -> do

            -- Create a new text tag table and a buffer for the text view
            tagTable <- Gtk.new Gtk.TextTagTable []
            txtBuffer <- Gtk.new Gtk.TextBuffer [#tagTable := tagTable]

            -- Create a TextTag for highlighting 'hello' word
            _ <- Highlighting.initializeHighlighting rules tagTable

            -- Create text view.
            textView <- Gtk.new Gtk.TextView [#buffer := txtBuffer]
            widgetShowAll textView -- must show before add notebook,
                                   -- otherwise notebook won't display child widget
                                   -- even have add in notebook.
            -- When the buffer content changes, check for instances of 'hello' and apply the tag
            _ <- Gtk.on txtBuffer #changed $ do
                Highlighting.applyRules rules separators txtBuffer

            -- Create notebook tab.
            tab <- notebookTabNew (Just "New tab") Nothing
            menuLabel <- labelNew (Nothing :: Maybe Text)

            -- Add widgets in notebook.
            notebookAppendPageMenu notebook textView (Just $ ntBox tab) (Just menuLabel)

            -- Start spinner animation when create tab.
            notebookTabStart tab

            -- Stop spinner animation after finish load.
            timeoutAdd PRIORITY_DEFAULT 5000 $ notebookTabStop tab >> return False

            -- Close tab when click button.
            onToolButtonClicked (ntCloseButton tab) $ do
              index <- notebookPageNum notebook textView
              notebookRemovePage notebook index
            return True
          _ -> return False
      _ -> return False

  -- Show window.
  box <- boxNew OrientationVertical 0
  boxPackStart box menuBar False False 0  -- Add the menu bar to the top of the window
  boxPackStart box notebook True True 0
  containerAdd window box
  widgetShowAll window
  onWidgetDestroy window mainQuit
  Gtk.main

