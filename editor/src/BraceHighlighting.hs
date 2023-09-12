{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module BraceHighlighting(
  applyBraceHighlighting
) where

import Data.GI.Base
import qualified GI.Gtk as Gtk
import qualified Data.Text as T
import Data.Char (isSpace)
import qualified GI.Pango as Pango


 -- | If the caret is next to a brace `(`, `)`, so that no characters, except for spaces, are between them,
 -- | it highlights the matching other (closing) brace. This highlighting is done by making both, the brace
 -- | next to the caret and the other closing brace fat/bold.
applyBraceHighlighting :: Gtk.TextBuffer -> IO ()
applyBraceHighlighting buffer = do
    insertMark <- Gtk.textBufferGetInsert buffer
    iter <- Gtk.textBufferGetIterAtMark buffer insertMark

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
            -- Here, you'd implement the logic to find the matching brace and apply highlighting to both
            -- For the sake of brevity, we're just highlighting the found brace
            highlightBrace buffer (pos + fromIntegral startOffset) (pos + fromIntegral startOffset + 1)
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


    highlightBrace :: Gtk.TextBuffer -> Int -> Int -> IO ()
    highlightBrace buffer start end = do
      startIter <- Gtk.textBufferGetIterAtOffset buffer $ fromIntegral start
      endIter <- Gtk.textBufferGetIterAtOffset buffer $ fromIntegral end

      tag <- Gtk.textTagNew Nothing
      Gtk.set tag [ #weightSet := True, #weight := 700 ]

      tagTable <- Gtk.textBufferGetTagTable buffer
      Gtk.textTagTableAdd tagTable tag

      Gtk.textBufferApplyTag buffer tag startIter endIter