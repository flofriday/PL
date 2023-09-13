{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
module ErrorHighlighting (
  checkSyntax,
  initializeErrorHighlighting
)where

import Data.GI.Base
import qualified GI.Gtk as Gtk
import qualified GI.Gtk as Gdk
import qualified Data.Text as T
import Data.Char (isSpace)
import qualified GI.Pango as Pango
import Control.Monad (when, forM, forM_)

import qualified Tokenizer
import qualified Parser


initializeErrorHighlighting :: Gtk.TextBuffer -> IO ()
initializeErrorHighlighting buffer = do
    tagTable <- #getTagTable buffer
    tag <- Gtk.new Gtk.TextTag [ #name := "Error"
                               , Gtk.textTagUnderline := Pango.UnderlineError
                               , Gtk.textTagUnderlineSet := True]

    _ <- #add tagTable tag
    return ()


checkSyntax :: Gtk.TextBuffer -> IO ()
checkSyntax buffer = do
--    start <- Gtk.textBufferGetStartIter buffer
--    end <- Gtk.textBufferGetEndIter buffer
--    text <- Gtk.textBufferGetText buffer start end True

    tokens <- Tokenizer.tokenizeTextBuffer $ buffer
    let (exprs, errors) = Parser.parseTokens tokens

    tagTable <- Gtk.textBufferGetTagTable buffer
    Just errTag <- #lookup tagTable $ T.pack "Error"

    forM_ errors $ \(errorMsg, (startOffset, endOffset)) -> do
        startIter <- Gtk.textBufferGetIterAtOffset buffer $ fromIntegral startOffset
        endIter <- Gtk.textBufferGetIterAtOffset buffer $ fromIntegral (endOffset + 1)
        Gtk.textBufferApplyTag buffer errTag startIter endIter

    print errors
