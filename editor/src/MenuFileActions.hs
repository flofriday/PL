{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module MenuFileActions(
    openFileDialog,
    saveFileDialog,
) where

import qualified GI.Gtk as Gtk
import Data.GI.Base



-- Function to display the open file dialog
openFileDialog :: IO ()
openFileDialog = do
    -- Create a new file chooser dialog
    dialog <- new Gtk.FileChooserDialog
                [#title := "Open File", #action := Gtk.FileChooserActionOpen]
    _ <- #addButton dialog
                  "Cancel"
                  (fromIntegral (fromEnum Gtk.ResponseTypeCancel))
    _ <- #addButton dialog "Open" (fromIntegral (fromEnum Gtk.ResponseTypeAccept))

    res <- #run dialog

    if res == 1
      then do
          -- TODO implement file read into buffer
          -- Just filename <- Gtk.fileChooserGetFilename dialog
          -- buffer <- Gtk.new Gtk.TextBuffer []
          -- Gtk.textBufferSetText buffer (unpack filename) (-1)
          return ()
      else return ()

    #destroy dialog

saveFileDialog :: IO ()
saveFileDialog = do
    dialog <- new Gtk.FileChooserDialog [#title := "Save File"]
    _ <- #addButton dialog
                  "Cancel"
                  (fromIntegral (fromEnum Gtk.ResponseTypeCancel))
    _ <- #addButton dialog "Save" (fromIntegral (fromEnum Gtk.ResponseTypeAccept))

    res <- #run dialog

    if res == 1
      then do
          -- TODO implement file save from buffer
          -- Just filename <- Gtk.fileChooserGetFilename dialog
          -- startIter <- Gtk.textBufferGetStartIter buffer
          -- endIter <- Gtk.textBufferGetEndIter buffer
          -- text <- Gtk.textBufferGetText buffer startIter endIter True
          -- writeFile (unpack filename) (unpack text)
          return ()
      else return ()
    Gtk.widgetDestroy dialog

