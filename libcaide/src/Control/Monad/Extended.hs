module Control.Monad.Extended(
      whenJust
    , whenM
    , unlessM
    , orThrow
    , module Control.Monad
    , module Control.Monad.Except
    , module Control.Monad.IO.Class
) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _ = pure ()
whenJust (Just a) f = f a

whenM :: Monad m => m Bool -> m () -> m ()
whenM condM action = do
    cond <- condM
    when cond action

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM condM action = do
    cond <- condM
    unless cond action

orThrow :: MonadError e m => Maybe a -> e -> m a
orThrow Nothing  e = throwError e
orThrow (Just a) _ = pure a

