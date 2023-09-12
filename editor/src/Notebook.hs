{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Notebook(
  notebookTabNew,
  notebookTabStart,
  notebookTabStop,
  NotebookTab(..)
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Exception (catch)
import Data.Maybe
import Data.Text (Text)
import GI.Gtk
       (containerRemove, IsContainer, boxReorderChild, widgetGetParent,
        IsWidget, IsBox, imageNewFromPixbuf, iconThemeLoadIcon,
        iconThemeGetDefault, Image, spinnerStop, widgetShow, spinnerStart,
        labelSetText, setWindowTitle, boxPackStart, toolButtonNew,
        spinnerNew, boxNew, mainQuit, onWidgetDestroy, containerAdd,
        notebookRemovePage, notebookPageNum, onToolButtonClicked,
        notebookAppendPageMenu, labelNew, widgetShowAll, textViewNew,
        windowSetPosition, windowSetDefaultSize,
        notebookNew, windowNew, ToolButton, Label, Spinner, Box
        ,menuItemNewWithLabel, menuItemNewWithMnemonic,
        onMenuItemActivate, menuShellAppend, menuItemSetSubmenu, menuNew,
        menuBarNew
        )
import qualified GI.Gtk as Gtk (main, init)
import GI.Gtk.Enums (Orientation(..))
import GI.Gtk.Flags (IconLookupFlags(..))

import GI.Gdk (keyvalName, getEventKeyKeyval)
import Data.GI.Base.Attributes (AttrOp(..), set)
import Data.GI.Base.BasicTypes (UnexpectedNullPointerReturn(..))
import Data.GI.Base
data NotebookTab =
    NotebookTab {ntBox          :: Box
                ,ntSpinner      :: Spinner
                ,ntLabel        :: Label
                ,ntCloseButton  :: ToolButton
                ,ntSize         :: Int}

-- | Create notebook tab.
notebookTabNew :: Maybe Text -> Maybe Int -> IO NotebookTab
notebookTabNew name size = do
  -- Init.
  let iconSize = fromMaybe 12 size
  box <- boxNew OrientationHorizontal 0
  spinner <- spinnerNew
  label <- labelNew name
  image <- imageNewFromIcon "window-close" iconSize
  closeButton <- toolButtonNew (Just image) (Nothing::Maybe Text)

  -- Show.
  boxPackStart box label False False 0
  boxPackStart box closeButton False False 0
  widgetShowAll box

  return $ NotebookTab box spinner label closeButton iconSize

-- | Set tab name.
notebookTabSetName :: NotebookTab -> Text -> IO ()
notebookTabSetName tab =
  labelSetText (ntLabel tab)

-- | Start spinner animation.
notebookTabStart :: NotebookTab -> IO ()
notebookTabStart NotebookTab {ntBox     = box
                             ,ntSpinner = spinner
                             ,ntSize    = size} = do
  boxTryPack box spinner False False (Just 0) (size `div` 2)
  spinnerStart spinner
  widgetShow spinner

-- | Stop spinner animation.
notebookTabStop :: NotebookTab -> IO ()
notebookTabStop NotebookTab {ntBox     = box
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


