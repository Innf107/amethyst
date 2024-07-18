module Sirius.Util where

import Relude

mapAccumLM :: (Traversable t, Monad m) => (s -> a -> m (s, b)) -> s -> t a -> m (s, t b)
mapAccumLM f initial traversable =
    fmap swap $ flip runStateT initial $ flip traverse traversable \x -> do
        state <- get
        (newState, newValue) <- lift $ f state x
        put newState
        pure newValue
