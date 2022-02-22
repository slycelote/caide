module Control.Monad.Extended(
      module Control.Monad
    , MonadIO
    , liftIO
    , whenJust
    , whenM
    , unlessM
) where

import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)

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

