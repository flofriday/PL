{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | This module provides functions to initialize and apply syntax highlighting rules
-- to a Gtk TextBuffer, using specified highlight conditions and rules.
module Highlighting (
  HighlightRule,
  HighlightCond(Keys, Expr),
  initializeHighlighting, -- ^ Initializes the highlighting by creating text tags for each rule and adding them to the text tag table.
  applyRules,            -- ^ Applies the highlighting rules to the text present in the buffer, based on the provided separators.
) where

import Data.GI.Base
import qualified GI.Gtk as Gtk
import qualified Data.Text as T
import Control.Monad (when, forM, forM_)

-- | Specifies the condition for highlighting a word: either being a member of a set of keywords (Keys),
-- or satisfying a given predicate function (Expr).
data HighlightCond = Keys [String] -- ^ A list of keyword strings to be highlighted.
                   | Expr (String -> Bool) -- ^ A predicate function to determine if a string should be highlighted.

-- | Defines a syntax highlighting rule, with a name, foreground color and a condition under which to apply the rule.
type HighlightRule = (String, String, HighlightCond)

type Separators = [Char] -- ^ A list of characters to be considered as word separators.
type WordInfo = (T.Text, Int, Int) -- ^ A tuple representing a word or separator in the text (string, start offset, end offset).

-- | Initializes highlighting by creating and adding text tags for each rule in the tag table.
initializeHighlighting :: [HighlightRule] -> Gtk.TextTagTable -> IO [Gtk.TextTag]
initializeHighlighting rules tagTable =
  forM rules $ \(name, foregroundColor, _) -> do
    tag <- Gtk.new Gtk.TextTag [#name := T.pack name, #foreground := T.pack foregroundColor]
    _ <- #add tagTable tag
    return tag

-- | Applies the highlighting rules to the entire text in the buffer, removing any existing tags first.
applyRules :: [HighlightRule] -> Separators -> Gtk.TextBuffer -> IO ()
applyRules rules separators buffer = do
  start <- #getStartIter buffer
  end <- #getEndIter buffer
  #removeAllTags buffer start end

  text <- #getText buffer start end True
  let foundWords = getWords text separators

  forM_ rules $ \(tagName, _, condition) -> do
    tagTable <- #getTagTable buffer
    maybeTag <- #lookup tagTable $ T.pack tagName
    case maybeTag of
      Just tag -> mapM_ (applyHighlight condition buffer tag) foundWords
      Nothing  -> return ()

-- | Applies a single highlight rule to a word in the text, based on the condition specified in the rule.
applyHighlight :: HighlightCond -> Gtk.TextBuffer -> Gtk.TextTag -> WordInfo -> IO ()
applyHighlight condition buffer tag (word, startOffset, endOffset) = do
  case condition of
    Keys keywords -> when (T.unpack word `elem` keywords) $ applyTag buffer tag startOffset endOffset
    Expr expr     -> when (expr $ T.unpack word) $ applyTag buffer tag startOffset endOffset

-- | Applies a text tag to a portion of text in the buffer, between the specified start and end offsets.
applyTag :: Gtk.TextBuffer -> Gtk.TextTag -> Int -> Int -> IO ()
applyTag buffer tag startOffset endOffset = do
  startIter <- #getIterAtOffset buffer $ fromIntegral startOffset
  endIter   <- #getIterAtOffset buffer $ fromIntegral (endOffset + 1)
  #applyTag buffer tag startIter endIter

-- | Splits the text into words and separators, with their respective start and end offsets.
getWords :: T.Text -> Separators -> [WordInfo]
getWords text separators = extractWords (0, T.unpack text)
  where
    extractWords (_, []) = []
    extractWords (i, xs) =
      let 
        -- This separates the input string 'xs' into a 'word' (sequence of characters not in 'separators') 
        -- and the 'rest' of the string.
        (word, rest) = span (`notElem` separators) xs 
      
        -- Calculate the length of the 'word'.
        wordLen = length word 
      
        -- If 'rest' is not empty, create a list containing a tuple with information about the separator (which is 
        -- the first character in 'rest'), otherwise create an empty list.
        sepInfo = [(T.pack [head rest], i + wordLen, i + wordLen) | not (null rest)]
      in 
        -- Create a tuple containing information about the 'word', add it to the list followed by 'sepInfo', and 
        -- recursively call 'extractWords' to process the rest of the string (excluding the first character which 
        -- is a separator).
        (T.pack word, i, i + wordLen - 1) : sepInfo ++ extractWords (i + wordLen + 1, if null rest then [] else tail rest)

