{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UIComponents(
  createEditorView
) where
import qualified GI.Gtk as Gtk

import Data.GI.Base ( AttrOp((:=)) )


-- TODO: Add UI Components
-- haskell-gi examples can be found here https://github.com/haskell-gi/gi-gtk-examples/tree/master

createEditorView :: Gtk.TextBuffer -> IO Gtk.ScrolledWindow
createEditorView txtBuffer = do
    scrollWindow <- Gtk.scrolledWindowNew (Nothing :: Maybe Gtk.Adjustment) (Nothing :: Maybe Gtk.Adjustment)
    textView <- Gtk.new Gtk.TextView [#buffer := txtBuffer, #leftMargin := 10, #topMargin := 10 ]

    -- add text view styling
    styleProvider <- Gtk.cssProviderNew
    Gtk.cssProviderLoadFromData styleProvider
        "textview text{ \
        \    caret-color: #FFFFFF; \
        \    background-color: #1E1F22; \
        \    color: #BCBEC4; \
        \} \
        \textview { \
        \    font-family: monospace; \
        \    font-size: 16px; \
        \}"
    screen <- Gtk.widgetGetScreen textView
    Gtk.styleContextAddProviderForScreen screen styleProvider 800

    Gtk.widgetShowAll textView -- must show before add notebook,
                           -- otherwise notebook won't display child widget
                           -- even have add in notebook.

    Gtk.containerAdd scrollWindow textView
    Gtk.widgetShowAll scrollWindow
    return scrollWindow
