{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MonoLocalBinds #-}
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
import qualified MenuFileActions
import MenuBar
import Data.GI.Base

-- | Main
main :: IO ()
main = do
  -- Init.
  _ <- Gtk.init Nothing

  -- Create window, notebook and menubar
  window <- windowNew WindowTypeToplevel
  notebook <- notebookNew
  menuBar <- createMenuBar $ menuBarDescr notebook

  -- Set window.
  windowSetDefaultSize window 800 600
  windowSetPosition window WindowPositionCenter
  setWindowTitle window "Editor"

  
  -- Handle key press action.
  _ <- onWidgetKeyPressEvent window $ \e ->
    -- Create new tab when user press Ctrl+n
    getEventKeyState e >>= \case
      [ModifierTypeControlMask] ->
        getEventKeyKeyval e >>= keyvalName >>= \case
          -- Save file keybinding
          Just "n" -> do
              Notebook.createAndAddDefaultTab notebook
              return True
          -- Open file keybinding
          Just "o" -> do
              MenuFileActions.openFileDialog notebook
              return True
          -- Save file keybinding
          Just "s" -> do
              -- MenuFileActions.saveFileDialog notebook
              return False
          _ -> return False
      _ -> return False

  -- Create a new text tag table and a buffer for the text view
  MenuFileActions.openWelcomeFile notebook

  -- TODO Handle notebook page switch

  -- Show window.
  box <- boxNew OrientationVertical 0
  boxPackStart box menuBar False False 0  -- Add the menu bar to the top of the window
  boxPackStart box notebook True True 0
  containerAdd window box
  widgetShowAll window
  _ <- onWidgetDestroy window mainQuit
  Gtk.main
