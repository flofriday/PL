module Tokenizer(
  tokenizeTextBuffer,
  Offset,
  Token(..),
) where


import Data.Char (isSpace, isAlpha, isDigit)
import qualified Data.Text as T
import GI.Gtk (TextBuffer, textBufferGetStartIter, textBufferGetEndIter, textBufferGetText)

-- Offset is the start and end (inclusive) offset of the token
type Offset = (Int, Int)
data Token = LetKeyword Offset
           | Identifier String Offset
           | IntegerLiteral Int Offset
           | BooleanLiteral Bool Offset
           | OpenParen Offset
           | CloseParen Offset
           | InvalidToken Char Offset
           | EOF
           deriving (Eq, Show)




tokenize :: T.Text -> [Token]
tokenize = tokenize' 0 []

tokenize' :: Int -> [Token] -> T.Text -> [Token]
tokenize' pos tokens text
    | T.null text = reverse tokens
    | otherwise = let (newToken, restText, newPos) = extractToken pos text
                  in tokenize' newPos (newToken : tokens) restText

getTextBufferContent :: TextBuffer -> IO T.Text
getTextBufferContent textBuffer = do
    startIter <- textBufferGetStartIter textBuffer
    endIter <- textBufferGetEndIter textBuffer
    textBufferGetText textBuffer startIter endIter True

tokenizeTextBuffer :: TextBuffer -> IO [Token]
tokenizeTextBuffer textBuffer = do
    content <- getTextBufferContent textBuffer
    return $ tokenize content

ignoreComment :: Int -> T.Text -> (T.Text, Int)
ignoreComment p text =
    case T.uncons text of
        Just ('\n', _) -> (T.tail text, p+1)
        Just (c, _) -> ignoreComment (p+1) $ T.tail text
        Nothing -> (text, p)

extractToken :: Int -> T.Text -> (Token, T.Text, Int)
extractToken pos text =
    case T.uncons text of
        Just ('#', _) -> let (ntext, npos) = ignoreComment (pos + 1) $ T.tail text
            in extractToken npos ntext
        Just ('(', _) -> (OpenParen (pos, pos), T.tail text, pos + 1)
        Just (')', _) -> (CloseParen (pos, pos), T.tail text, pos + 1)
        Just (c, rest)
            | isSpace c -> extractToken (pos + 1) rest
            | isAlpha c || c == '-' ->
                let (tokenStr, restText) = T.span (\x -> isAlpha x || isDigit x || x == '-') text
                    tokenLen = T.length tokenStr
                    newToken
--                      | "let" == T.unpack tokenStr   = LetKeyword (pos, pos + tokenLen - 1)
                      | "true" == T.unpack tokenStr  = BooleanLiteral True (pos, pos + tokenLen - 1)
                      | "false" == T.unpack tokenStr = BooleanLiteral False (pos, pos + tokenLen - 1)
                      | otherwise               = Identifier (T.unpack tokenStr) (pos, pos + tokenLen - 1)
                in (newToken, restText, pos + tokenLen)
            | isDigit c ->
                let (numStr, restText) = T.span isDigit text
                    numLen = T.length numStr
                in (IntegerLiteral (read $ T.unpack numStr) (pos, pos + numLen - 1), restText, pos + numLen)
            | True -> (InvalidToken c (pos, pos), rest, pos + 1)
        Nothing -> (EOF, T.pack "", pos)
