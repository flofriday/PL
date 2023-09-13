{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module IdentifierHighlighting
  ( initializeIdentifierHighlighting,
    applyIdentifierHighlighting,
  )
where

import Data.GI.Base
import qualified GI.Gtk as Gtk
import qualified Data.Text as T
import Highlighting (WordInfo, Separators, getWords)

initializeIdentifierHighlighting :: Gtk.TextBuffer -> IO ()
initializeIdentifierHighlighting buffer = do
  tagTable <- #getTagTable buffer
  tag <- Gtk.new Gtk.TextTag [#name := "IdentifierMatch", #background := "#43454A"]
  _ <- #add tagTable tag
  return ()

-- | If the caret is on a word, it highlights all occurrences of that word.
applyIdentifierHighlighting :: Gtk.TextBuffer -> Gtk.TextIter -> Separators -> IO ()
applyIdentifierHighlighting buffer iter separators = do
  -- Remove previous identifier highlights
  bufferStartIter <- #getStartIter buffer
  bufferEndIter <- #getEndIter buffer
  Gtk.textBufferRemoveTagByName buffer (T.pack "IdentifierMatch") bufferStartIter bufferEndIter

  -- Get the text around the caret
  startIter <- Gtk.textIterCopy iter
  _ <- Gtk.textIterBackwardChars startIter 1000
  endIter <- Gtk.textIterCopy iter
  _ <- Gtk.textIterForwardChars endIter 1000
  surroundingText <- Gtk.textBufferGetText buffer startIter endIter True

  -- Get the caret offset
  caretOffset <- Gtk.textIterGetOffset iter
  startOffset <- Gtk.textIterGetOffset startIter

  -- Split the text into words and separators
  let wordInfos = getWords surroundingText separators

  -- Find the word under the caret
  let wordUnderCaret = findWordUnderCaret wordInfos (fromIntegral caretOffset - fromIntegral startOffset)

  -- Highlight the word under the caret
  case wordUnderCaret of
    Just (wordText, start, end) -> do
      let startPos = start + fromIntegral startOffset
      let endPos = end + fromIntegral startOffset + 1
      highlightWord buffer startPos endPos
    Nothing -> return ()

  where
    findWordUnderCaret :: [WordInfo] -> Int -> Maybe WordInfo
    findWordUnderCaret [] _ = Nothing
    findWordUnderCaret (wordInfo:rest) caretPos =
      let
        (wordText, start, end) = wordInfo
      in
        if start <= caretPos && caretPos <= end then Just wordInfo else findWordUnderCaret rest caretPos

    highlightWord :: Gtk.TextBuffer -> Int -> Int -> IO ()
    highlightWord buffer start end = do
      startIter <- Gtk.textBufferGetIterAtOffset buffer (fromIntegral start)
      endIter <- Gtk.textBufferGetIterAtOffset buffer (fromIntegral end)
      tagTable <- Gtk.textBufferGetTagTable buffer
      Just tag <- Gtk.textTagTableLookup tagTable "IdentifierMatch"
      Gtk.textBufferApplyTag buffer tag startIter endIter
