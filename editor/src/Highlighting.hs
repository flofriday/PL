{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Highlighting (
  HighlightRule,
  HighlightCond(Keys, Expr),
  initializeHighlighting,
  applyRules,
) where

import Data.GI.Base
import qualified GI.Gtk as Gtk
import qualified Data.Text as T
import Control.Monad (when, forM)

-- condition is either one of the keywords in Keys [String] or the expression Expr evaluates to true
data HighlightCond = Keys [String]
                    | Expr (String -> Bool)

-- ( tagName, color, condition
type HighlightRule = (String, String, HighlightCond)

type Separators = [Char]
type WordInfo = (T.Text, Int, Int)


initializeHighlighting:: [HighlightRule] -> Gtk.TextTagTable -> IO [Gtk.TextTag]
initializeHighlighting rules tagTable = do
  forM rules $ \(name, foregroundColor, _) -> do
    tag <- Gtk.new Gtk.TextTag [#name := T.pack name, #foreground := T.pack foregroundColor]
    _ <- #add tagTable tag
    return tag


-- applies highlights to text buffer based on found words that hold start and end iterator offsets
applyRules :: [HighlightRule] -> Separators -> Gtk.TextBuffer -> IO ()
applyRules rules separators buffer = do
  start <- #getStartIter buffer
  end <- #getEndIter buffer
  #removeAllTags buffer start end

  text <- #getText buffer start end True
  let foundWords = getWords text separators

  mapM_ (applyRule foundWords) rules
  where
    applyRule :: [WordInfo] -> HighlightRule -> IO ()
    applyRule foundWords (tagName, _, condition) = do
      tagTable <- #getTagTable buffer
      maybeTag <- #lookup tagTable $ T.pack tagName
      case maybeTag of
        Just tag -> mapM_ (applyHighlight condition tag) foundWords
        Nothing  -> return ()

    applyHighlight:: HighlightCond -> Gtk.TextTag -> WordInfo -> IO ()
    applyHighlight condition tag (word, startOffset, endOffset) = do
      case condition of
        Keys keywords -> when (T.unpack word `elem` keywords) (applyTag tag startOffset endOffset)
        Expr expr     -> when (expr $ T.unpack word) (applyTag tag startOffset endOffset)

    applyTag:: Gtk.TextTag -> Int -> Int -> IO ()
    applyTag tag startOffset endOffset = do
      startIter <- #getIterAtOffset buffer $ fromIntegral startOffset
      endIter   <- #getIterAtOffset buffer $ fromIntegral (endOffset + 1)
      #applyTag buffer tag startIter endIter



getWords :: T.Text -> [Char] -> [WordInfo]
getWords text separators = addLastWord $ foldl processChar (0, [], 0) (zip [0..] (T.unpack text))
  where
    processChar (wordLen, foundWords, startIndex) (i, c)
      | c `elem` separators = if not (T.null currentWord)
                                 then (0, foundWords ++ [(currentWord, startIndex, endIndex), (T.pack [c], i, i)], i + 1)
                                 else (0, foundWords ++ [(T.pack [c], i, i)], i + 1)
      | otherwise = (wordLen + 1, foundWords, if T.null currentWord then i else startIndex)
      where
        endIndex = i - 1
        currentWord = T.take (fromIntegral wordLen) (T.drop (fromIntegral startIndex) text)

    addLastWord :: (Int, [WordInfo], Int) -> [WordInfo]
    addLastWord (wordLen, foundWords, startIndex) =
      let endIndex = T.length text - 1
          lastWord = T.take (endIndex - startIndex + 1) (T.drop startIndex text)
      in if not (T.null lastWord) && wordLen > 0
           then foundWords ++ [(lastWord, startIndex, endIndex)]
           else foundWords

