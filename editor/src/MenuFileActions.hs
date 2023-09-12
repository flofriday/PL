{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use when" #-}

module MenuFileActions
  ( openFileDialog,
    saveFileDialog,
  )
where

import Data.GI.Base (AttrOp ((:=)), new)
import Data.Text (pack, unpack)
import qualified GI.Gtk as Gtk
import qualified Notebook
import System.FilePath (takeFileName)

-- Function to display the open file dialog and perform an open
openFileDialog :: Gtk.Notebook -> IO ()
openFileDialog notebook = do
  -- Create a new file chooser dialog
  dialog <-
    new
      Gtk.FileChooserDialog
      [#title := "Open File", #action := Gtk.FileChooserActionOpen]
  _ <-
    #addButton
      dialog
      "Cancel"
      (fromIntegral (fromEnum Gtk.ResponseTypeCancel))
  _ <- #addButton dialog "Open" (fromIntegral (fromEnum Gtk.ResponseTypeAccept))

  res <- #run dialog

  if res == fromIntegral (fromEnum Gtk.ResponseTypeAccept)
    then do
      Just filePath <- Gtk.fileChooserGetFilename dialog
      let justFilename = takeFileName filePath
      tagTable <- Gtk.new Gtk.TextTagTable []
      buffer <- Gtk.new Gtk.TextBuffer [#tagTable := tagTable]
      contents <- readFile filePath
      let textContents = pack contents -- Convert String to Text
      Gtk.textBufferSetText buffer textContents (-1)
      -- TODO prevent opening same file twice
      _ <- Notebook.createAndAddTab notebook (pack justFilename) buffer tagTable
      return ()
    else return ()

  #destroy dialog

-- Function to display the save file dialog and perform a save
saveFileDialog :: Gtk.Notebook -> IO ()
saveFileDialog notebook = do
  dialog <- new Gtk.FileChooserDialog [#title := "Save File", #action := Gtk.FileChooserActionSave]
  #setDoOverwriteConfirmation dialog True
  _ <-
    #addButton
      dialog
      "Cancel"
      (fromIntegral (fromEnum Gtk.ResponseTypeCancel))
  _ <- #addButton dialog "Save" (fromIntegral (fromEnum Gtk.ResponseTypeAccept))

  res <- #run dialog

  if res == fromIntegral (fromEnum Gtk.ResponseTypeAccept)
    then do
      Just textBuffer <- Notebook.getCurrentBuffer notebook
      startIter <- #getStartIter textBuffer
      endIter <- #getEndIter textBuffer
      text <- Gtk.textBufferGetText textBuffer startIter endIter True
      Just filePath <- Gtk.fileChooserGetFilename dialog
      writeFile filePath $ unpack text
      return ()
    else return ()

  #destroy dialog
