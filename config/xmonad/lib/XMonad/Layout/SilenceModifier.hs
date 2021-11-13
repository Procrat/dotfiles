{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module XMonad.Layout.SilenceModifier (
    silenceLayoutModifier
) where


import           XMonad
import           XMonad.Layout.LayoutModifier (LayoutModifier (..),
                                               ModifiedLayout (..))


silenceLayoutModifier :: LayoutClass l w
                      => (l w -> ModifiedLayout lm l w)
                      -> l w
                      -> ModifiedLayout (SilencedLayoutModifier (ModifiedLayout lm l w)) l w
silenceLayoutModifier applyInner layout =
    ModifiedLayout (SilencedLayoutModifier (applyInner layout)) layout

newtype SilencedLayoutModifier inner window = SilencedLayoutModifier inner
    deriving (Read, Show)

instance (LayoutModifier inner Window, LayoutClass layout Window, Read (layout Window))
  => LayoutModifier (SilencedLayoutModifier (ModifiedLayout inner layout Window)) Window where
    -- The default implementation of modifierDescription is "", so we don't
    -- need to implement it

    redoLayout (SilencedLayoutModifier (ModifiedLayout inner layout)) screen ws windows = do
        (newWindows, maybeNewInner) <- redoLayout inner screen ws windows
        let maybeNewLayoutModifier = fmap (\newInner -> SilencedLayoutModifier (ModifiedLayout newInner layout)) maybeNewInner
        return (newWindows, maybeNewLayoutModifier)
