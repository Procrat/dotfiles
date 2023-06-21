{-# LANGUAGE FlexibleInstances     #-}
-- {-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE StandaloneDeriving    #-}

module XMonad.Layout.IconTabs (
    iconTabbed
) where

-- import           Control.Monad                (forM)
-- import           Data.Monoid                  (All (..))
-- import           Data.Set                     (Set)
import           Data.Map                     (Map)
-- import qualified Data.Set                     as Set
import qualified Data.Map                     as Map

import           Graphics.X11                 (Rectangle (Rectangle))
import           Graphics.X11.Xlib.Extras     (Event (ButtonEvent), ev_window)
import           XMonad                       (Window, fromMessage, io)
import qualified XMonad.Layout.LayoutModifier as LM
import           XMonad.Util.XUtils           (createNewWindow)
-- import qualified XMonad.StackSet              as W
-- import           XMonad.Util.XUtils           (fi)


iconTabbed :: layout window -> LM.ModifiedLayout IconTabbed layout window
iconTabbed = LM.ModifiedLayout (IconTabbed Map.empty)

newtype IconTabbed window = IconTabbed
    { iconPaths       :: Map window String
    } deriving (Read)
deriving instance Show window => Show (IconTabbed window)
-- TODO Are Show and Read necessary?


instance LM.LayoutModifier IconTabbed Window where
    handleMess _iconTabber message
      | Just ButtonEvent { ev_window } <- fromMessage message = do
            -- bar <- createNewWindow (Rectangle 20 20 40 40) Nothing "red" False
            bar <- createNewWindow (Rectangle 100 100 200 200) Nothing "red" True
            io $ print ev_window
            io $ print bar
            return Nothing
    handleMess _ _ = return Nothing

    -- redoLayout :: PseudoTiling Window
    --              -> Rectangle
    --              -> Maybe (W.Stack Window)
    --              -> [(Window, Rectangle)]
    --              -> X ([(Window, Rectangle)], Maybe (PseudoTiling Window))
    -- redoLayout PseudoTiling { pseudoWindows } _ _ windowRectangles = do
    --     newWindowRectangles <- forM windowRectangles $ \(window, rectangle) -> do
    --         preferredDimensions <- getPreferredDimensions window
    --         let newRectangle = if Set.member window pseudoWindows
    --             then pseudoTileDimension preferredDimensions rectangle
    --             else rectangle
    --         return (window, newRectangle)
    --     return (newWindowRectangles, Nothing)

    -- handleMess :: PseudoTiling Window
    --            -> SomeMessage
    --            -> X (Maybe (PseudoTiling Window))
    -- handleMess pseudoTiler@PseudoTiling { pseudoWindows } message
    --     | Just (SetWindow window) <- fromMessage message =
    --         return . Just $ pseudoTiler {
    --             pseudoWindows = Set.insert window pseudoWindows
    --         }
    --     | Just (ToggleWindow window) <- fromMessage message =
    --         return . Just $ pseudoTiler {
    --             pseudoWindows = toggleMember window pseudoWindows
    --         }
    --     | Just DestroyWindowEvent { ev_window = window } <- fromMessage message =
    --         return . Just $ pseudoTiler {
    --             pseudoWindows = Set.delete window pseudoWindows
    --         }
    --   where
    --     toggleMember el set = if Set.member el set
    --         then Set.delete el set
    --         else Set.insert el set
    -- handleMess _ _ = return Nothing
