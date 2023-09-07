{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Main (main) where

import qualified Highlighting
import Highlighting(HighlightCond(Keys, Expr))

import qualified Text.Read as TR
import Data.Maybe (isJust)

import Data.GI.Base
import qualified GI.Gtk as Gtk

main :: IO ()
main = do
  _ <- Gtk.init Nothing

  -- Create new top level window.
  window <- Gtk.new Gtk.Window []
  #setTitle window "Hello world"
  #setDefaultSize window 600 400

  -- Create a new text tag table and a buffer for the text view
  tagTable <- Gtk.new Gtk.TextTagTable []
  txtBuffer <- Gtk.new Gtk.TextBuffer [#tagTable := tagTable]

  ------ create highlight rules -----
  -- number recognitions lambda
  let isStringInt = isJust . (TR.readMaybe :: String -> Maybe Int)
  -- highlight rules
  let rules =
        [ ("keywords", "blue", Keys ["func"])
        , ("number_literals", "red", Expr isStringInt)
        ]
  -- word separators
  let separators = [' ', '(', ')', '\n']

  -- Create a TextTag for highlighting 'hello' word
  _ <- Highlighting.initializeHighlighting rules tagTable

  -- Create a new text view widget
  txtView <- Gtk.new Gtk.TextView [#buffer := txtBuffer]
  Gtk.widgetShow txtView


  -- When the buffer content changes, check for instances of 'hello' and apply the tag
  _ <- Gtk.on txtBuffer #changed $ do
      Highlighting.applyRules rules separators txtBuffer

  -- Add the text view to the window and display everything
  Gtk.containerAdd window txtView
  Gtk.widgetShow window
  Gtk.main