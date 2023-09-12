{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Main(main) where

import GI.Gtk
       (setWindowTitle, boxPackStart,
        boxNew, mainQuit, onWidgetDestroy, containerAdd,
        widgetShowAll,
        onWidgetKeyPressEvent, windowSetPosition, windowSetDefaultSize,
        notebookNew, windowNew,
        )
import GI.Gtk.Enums (Orientation(..), WindowType(..), WindowPosition(..))
import GI.Gdk (keyvalName, getEventKeyKeyval, getEventKeyState)
import GI.Gdk.Flags (ModifierType(..))

import qualified GI.Gtk as Gtk

import qualified Notebook
import qualified Highlighting
import MenuBar


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
              Notebook.createAndAddTab notebook Highlighting.rules Highlighting.separators "New Tab"
          _ -> return False
      _ -> return False

  -- Create one open tab
  _ <- Notebook.createAndAddTab notebook Highlighting.rules Highlighting.separators "New Tab"

  -- Show window.
  box <- boxNew OrientationVertical 0
  boxPackStart box menuBar False False 0  -- Add the menu bar to the top of the window
  boxPackStart box notebook True True 0
  containerAdd window box
  widgetShowAll window
  _ <- onWidgetDestroy window mainQuit
  Gtk.main
