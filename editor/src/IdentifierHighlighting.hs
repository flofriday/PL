{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module IdentifierHighlighting
  ( initializeIdentifierHighlighting,
    applyIdentifierHighlighting,
  )
where

import Data.GI.Base ( AttrOp((:=)) )
import qualified GI.Gtk as Gtk
import qualified Data.Text as T
import Highlighting (WordInfo, Separators, getWords)
import Data.Text.Read (decimal)

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
  let wordList = getWords surroundingText separators

  -- Find the word under the caret
  let wordUnderCaret = findWordUnderCaret wordList (fromIntegral caretOffset - fromIntegral startOffset)
  
  case wordUnderCaret of
    Just (text, _, _) -> do
      let filteredList = filterWordListByText text wordList
      highlightWords buffer filteredList
    Nothing -> return ()

  where
    filterWordListByText :: T.Text -> [WordInfo] -> [WordInfo]
    filterWordListByText targetText = filter (\(text, _, _) -> text == targetText)

    isNumber :: T.Text -> Bool
    isNumber text = case decimal text of
      Right _ -> True
      _ -> False

    isSeparatorWord :: T.Text -> Bool
    isSeparatorWord text = all (`elem` separators) (T.unpack text)

    findWordUnderCaret :: [WordInfo] -> Int -> Maybe WordInfo
    findWordUnderCaret [] _ = Nothing
    findWordUnderCaret (wordInfo : rest) caretPos =
      let
        (wordText, start, end) = wordInfo
      in
        if start <= caretPos && caretPos <= end then
          if isNumber wordText || isSeparatorWord wordText then
            Nothing -- Return Nothing if it's a number or contains only separators
          else
            Just wordInfo -- Return Just wordInfo otherwise
        else
          findWordUnderCaret rest caretPos

    highlightWords :: Gtk.TextBuffer -> [WordInfo] -> IO ()
    highlightWords _buffer wordInfoList = do
      tagTable <- Gtk.textBufferGetTagTable _buffer
      Just tag <- Gtk.textTagTableLookup tagTable "IdentifierMatch"
      mapM_ (highlightWord buffer tag) wordInfoList

    highlightWord :: Gtk.TextBuffer -> Gtk.TextTag -> WordInfo -> IO ()
    highlightWord _buffer tag (_, start, end) = do
      startIter <- Gtk.textBufferGetIterAtOffset _buffer (fromIntegral start)
      endIter <- Gtk.textBufferGetIterAtOffset _buffer (fromIntegral end + 1)
      Gtk.textBufferApplyTag _buffer tag startIter endIter
