{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module XMonad.Layout.SingleSpacing (
    spacing
) where


import Control.Arrow (second)
import Debug.Trace
import Graphics.X11 (Rectangle(..))
import XMonad.Layout.LayoutModifier
import XMonad.Util.XUtils (fi)


spacing :: Int -> l a -> ModifiedLayout SingleSpacing l a
spacing pixels = ModifiedLayout $ SingleSpacing pixels


newtype SingleSpacing a = SingleSpacing Int deriving (Read, Show)


instance LayoutModifier SingleSpacing a where
    pureModifier (SingleSpacing pixels) screen _ windows =
        (map (second $ shrink pixels screen) windows, Nothing)


shrink :: Int -> Rectangle -> Rectangle -> Rectangle
shrink pixels (Rectangle screenX screenY _ _)
              (Rectangle winX winY winWidth winHeight) =
    Rectangle (winX + offsetX) (winY + offsetY)
              (winWidth - fi pixels - fi offsetX)
              (winHeight - fi pixels - fi offsetY)
    where offsetX = if winX == screenX then fi pixels else 0
          offsetY = if winY == screenY then fi pixels else 0
