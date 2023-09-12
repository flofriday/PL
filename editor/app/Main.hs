{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Main(main) where

import Data.Maybe
import Data.Text (Text, pack, unpack)
import GI.Gtk
       (setWindowTitle, boxPackStart,
        boxNew, mainQuit, onWidgetDestroy, containerAdd,
        notebookRemovePage, notebookPageNum, onToolButtonClicked,
        notebookAppendPageMenu, labelNew, widgetShowAll,
        onWidgetKeyPressEvent, windowSetPosition, windowSetDefaultSize,
        notebookNew, windowNew,
        )
import GI.Gtk.Enums (Orientation(..), WindowType(..), WindowPosition(..))
import GI.Gdk (keyvalName, getEventKeyKeyval, getEventKeyState)
import GI.Gdk.Flags (ModifierType(..))
import GI.Gdk.Structs.RGBA (RGBA, newZeroRGBA, rGBAParse)
import GI.GLib (timeoutAdd, pattern PRIORITY_DEFAULT)

import qualified Highlighting
import Highlighting(HighlightCond(Keys, Expr))

import qualified Text.Read as TR

import Data.GI.Base
import qualified GI.Gtk as Gtk


import Notebook
import UIComponents

-- HIGHLIGHTING

-- number recognitions lambda
isStringInt :: String -> Bool
isStringInt = isJust . (TR.readMaybe :: String -> Maybe Int)

-- highlight rules
rules :: [(String, String, HighlightCond)]
rules =
      [ ("keywords", "#C08E71", Keys ["lambda", "let", "defun"])
      , ("number_literals", "#55A9B6", Expr isStringInt)
      , ("brackets", "#BD80B8", Keys ["(", ")"])
      , ("boolean", "#6BA6EF", Keys ["true", "false"])
      ]

-- word separators
separators :: [Char]
separators = [' ', '(', ')', '\n']

-- | Main
main :: IO ()
main = do
  -- Init.
  _ <- Gtk.init Nothing

  -- Create window, notebook and menubar
  window <- windowNew WindowTypeToplevel
  notebook <- notebookNew
  menuBar <- createMenuBar menuBarDescr  

  -- Set window.
  windowSetDefaultSize window 800 600
  windowSetPosition window WindowPositionCenter
  setWindowTitle window "Hello World."

  -- Handle key press action.
  _ <- onWidgetKeyPressEvent window $ \e ->
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


            -- Create editor view
            editorView <- createEditorView txtBuffer

            -- When the buffer content changes, check for instances of 'hello' and apply the tag
            _ <- Gtk.on txtBuffer #changed $ do
                Highlighting.applyRules rules separators txtBuffer

            -- Create notebook tab.
            tab <- notebookTabNew (Just "New tab") Nothing
            menuLabel <- labelNew (Nothing :: Maybe Text)

            -- Add widgets in notebook.
            _ <- notebookAppendPageMenu notebook editorView (Just $ ntBox tab) (Just menuLabel)

            -- Start spinner animation when create tab.
            notebookTabStart tab

            -- Stop spinner animation after finish load.
            _ <- timeoutAdd PRIORITY_DEFAULT 5000 $ notebookTabStop tab >> return False

            -- Close tab when click button.
            _ <- onToolButtonClicked (ntCloseButton tab) $ do
              index <- notebookPageNum notebook editorView
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
  _ <- onWidgetDestroy window mainQuit
  Gtk.main

