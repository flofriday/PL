{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MonoLocalBinds #-}

module Notebook(
  createAndAddTab,
  tabSetName,
) where

import Control.Monad
import Control.Exception (catch)
import Data.Maybe
import Data.Text (Text)
import GI.Gtk
       (containerRemove, IsContainer, boxReorderChild, widgetGetParent,
        IsWidget, IsBox, imageNewFromPixbuf, iconThemeLoadIcon,
        iconThemeGetDefault, Image, spinnerStop, widgetShow, spinnerStart,
        labelSetText, boxPackStart, toolButtonNew,
        spinnerNew, boxNew, labelNew, widgetShowAll, ToolButton, Label, Spinner, Box
        
        )
import GI.GLib (timeoutAdd, pattern PRIORITY_DEFAULT)
import GI.Gtk.Enums (Orientation(..))
import GI.Gtk.Flags (IconLookupFlags(..))
import qualified GI.Gtk as Gtk
import Data.GI.Base
import qualified Highlighting
import qualified UIComponents

data NotebookTab =
    NotebookTab {ntBox          :: Box
                ,ntSpinner      :: Spinner
                ,ntLabel        :: Label
                ,ntCloseButton  :: ToolButton
                ,ntSize         :: Int}

-- Create new tab and link with highlighter
createAndAddTab :: Gtk.Notebook -> Text -> Gtk.TextBuffer -> Gtk.TextTagTable -> IO Bool
createAndAddTab notebook name txtBuffer tagTable = do
    -- Create a TextTag for highlighting 'hello' word
    _ <- Highlighting.initializeHighlighting Highlighting.rules tagTable

    -- Create text view.
    textView <- UIComponents.createEditorView txtBuffer
    Gtk.widgetShowAll textView -- must show before adding to the notebook
    -- When the buffer content changes, check for instances of 'hello' and apply the tag
    _ <- Gtk.on txtBuffer #changed $ do
        Highlighting.applyRules Highlighting.rules Highlighting.separators txtBuffer

    -- Create notebook tab.
    tab <- tabNew (Just name) Nothing
    menuLabel <- labelNew (Nothing :: Maybe Text)

    -- Add widgets in notebook.
    _ <- Gtk.notebookAppendPageMenu notebook textView (Just $ ntBox tab) (Just menuLabel)

    -- Start spinner animation when creating the tab.
    tabStart tab

    -- Stop spinner animation after finishing loading.
    _ <- timeoutAdd PRIORITY_DEFAULT 5000 $ tabStop tab >> return False

    -- Close tab when clicking the button.
    _ <- Gtk.onToolButtonClicked (ntCloseButton tab) $ do
        index <- Gtk.notebookPageNum notebook textView
        Gtk.notebookRemovePage notebook index

    return True


-- | Create notebook tab.
tabNew :: Maybe Text -> Maybe Int -> IO NotebookTab
tabNew name size = do
  -- Init.
  let iconSize = fromMaybe 0 size
  box <- boxNew OrientationHorizontal 0
  spinner <- spinnerNew
  label <- labelNew name
  image <- imageNewFromIcon "window-close" iconSize
  closeButton <- toolButtonNew (Just image) (Nothing::Maybe Text)

  -- Show.
  boxPackStart box label False False 0
  boxPackStart box closeButton False False 5
  widgetShowAll box

  return $ NotebookTab box spinner label closeButton iconSize

-- | Set tab name.
tabSetName :: NotebookTab -> Text -> IO ()
tabSetName tab =
  labelSetText (ntLabel tab)

-- | Start spinner animation.
tabStart :: NotebookTab -> IO ()
tabStart NotebookTab {ntBox     = box
                             ,ntSpinner = spinner
                             ,ntSize    = size} = do
  boxTryPack box spinner False False (Just 0) (size `div` 5)
  spinnerStart spinner
  widgetShow spinner

-- | Stop spinner animation.
tabStop :: NotebookTab -> IO ()
tabStop NotebookTab {ntBox     = box
                            ,ntSpinner = spinner} = do
  containerTryRemove box spinner
  spinnerStop spinner

-- | Create image widget with given icon name and size.
imageNewFromIcon :: Text -> Int -> IO Image
imageNewFromIcon iconName size = do
  iconTheme <- iconThemeGetDefault
  -- Function 'iconThemeLoadIcon' can scal  e icon with specified size.
  pixbuf <- fromJust <$> iconThemeLoadIcon iconTheme iconName (fromIntegral size) [IconLookupFlagsUseBuiltin]
  imageNewFromPixbuf (Just pixbuf)

-- | Try to packing widget in box.
-- If @child@ have exist parent, do nothing,
-- otherwise, add @child@ to @parent@.
boxTryPack :: (IsBox parent, IsWidget child) => parent -> child -> Bool -> Bool -> Maybe Int -> Int -> IO ()
boxTryPack box widget expand fill order space =
    void (widgetGetParent widget)
  `catch` (\(_ :: UnexpectedNullPointerReturn) -> do
    boxPackStart box widget expand fill (fromIntegral space)
    order ?>= (boxReorderChild box widget . fromIntegral))

-- | Try to remove child from parent.
containerTryRemove :: (IsContainer parent, IsWidget child) => parent -> child -> IO ()
containerTryRemove parent widget = do
  hasParent <- (widgetGetParent widget >> return True) `catch` (\(_ :: UnexpectedNullPointerReturn) -> return False)
  when hasParent $ containerRemove parent widget

-- | Maybe.
(?>=) :: Monad m => Maybe a -> (a -> m ()) -> m ()
m ?>= f = maybe (return ()) f m


