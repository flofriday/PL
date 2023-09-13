{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module BraceHighlighting(
  initializeBraceHighlighting,
  applyBraceHighlighting,
) where

import Data.GI.Base
import qualified GI.Gtk as Gtk
import qualified Data.Text as T
import Data.Char (isSpace)
import qualified GI.Pango as Pango
import Control.Monad (when)

initializeBraceHighlighting :: Gtk.TextBuffer -> IO ()
initializeBraceHighlighting buffer = do
  tagTable <- #getTagTable buffer
  tag <- Gtk.new Gtk.TextTag [#name := "BraceMatch", #background := "#43454A"]
  _ <- #add tagTable tag
  return ()


 -- | If the caret is next to a brace `(`, `)`, so that no characters, except for spaces, are between them,
 -- | it highlights the matching other (closing) brace. This highlighting is done by making both, the brace
 -- | next to the caret and the other closing brace fat/bold.
applyBraceHighlighting :: Gtk.TextBuffer -> Gtk.TextIter -> Gtk.TextMark -> IO ()
applyBraceHighlighting buffer iter insertMark = do
    -- remove brace highlight
    bufferStartIter <- #getStartIter buffer
    bufferEndIter <- #getEndIter buffer
    Gtk.textBufferRemoveTagByName buffer (T.pack "BraceMatch") bufferStartIter bufferEndIter
    
    -- Get the characters surrounding the caret (we assume a reasonable limit like 1000 characters for simplicity)
    startIter <- Gtk.textIterCopy iter
    _ <- Gtk.textIterBackwardChars startIter 1000
    endIter <- Gtk.textIterCopy iter
    _ <- Gtk.textIterForwardChars endIter 1000

    surroundingText <- Gtk.textBufferGetText buffer startIter endIter True

    -- Find the position of the caret within the surrounding text
    caretOffset <- Gtk.textIterGetOffset iter
    startOffset <- Gtk.textIterGetOffset startIter
    let relativeCaretPos = caretOffset - startOffset

    -- Check for braces around the caret and highlight them along with their matching pair
    case findBracePos (T.unpack surroundingText) $ fromIntegral relativeCaretPos of
        Just pos -> do
            let (matchingPos, isMatchingFound) = findMatchingBrace (T.unpack surroundingText) pos
            when isMatchingFound $ do
                highlightBrace buffer (pos + fromIntegral startOffset) (pos + fromIntegral startOffset + 1)
                highlightBrace buffer (matchingPos + fromIntegral startOffset) (matchingPos + fromIntegral startOffset + 1)
        Nothing -> return ()
    where
      findBracePos :: String -> Int -> Maybe Int
      findBracePos text caretPos =
          let
              checkPositions = [caretPos - 1, caretPos, caretPos + 1]
              findPosition positions text = case positions of
                  [] -> Nothing
                  (p:ps) -> if isValidPos p && (text !! p == '(' || text !! p == ')') && not (isSpace (text !! p))
                            then Just p
                            else findPosition ps text
              isValidPos pos = pos >= 0 && pos < length text
          in
          findPosition checkPositions text
   
      findMatchingBrace :: String -> Int -> (Int, Bool)
      findMatchingBrace text pos =
        let
          brace = text !! pos
          matchingBrace
            | brace == '(' = ')'
            | brace == ')' = '('
            | otherwise = ' '
          findMatch count i
            | i < 0 || i >= length text = (i, False)
            | text !! i == brace = findMatch (count + 1) (if brace == '(' then i+1 else i-1)
            | text !! i == matchingBrace = if count == 1 then (i, True) else findMatch (count - 1) (if brace == '(' then i+1 else i-1)
            | otherwise = findMatch count (if brace == '(' then i+1 else i-1)
        in
        findMatch 1 (if brace == '(' then pos+1 else pos-1)
  
      highlightBrace :: Gtk.TextBuffer -> Int -> Int -> IO ()
      highlightBrace buffer start end = do
        startIter <- Gtk.textBufferGetIterAtOffset buffer $ fromIntegral start
        endIter <- Gtk.textBufferGetIterAtOffset buffer $ fromIntegral end
        tagTable <- #getTagTable buffer
        Just tag <- #lookup tagTable $ T.pack "BraceMatch"
        Gtk.textBufferApplyTag buffer tag startIter endIter
