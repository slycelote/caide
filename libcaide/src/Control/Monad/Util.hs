module Control.Monad.Util(
      whenJust
) where

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _ = pure ()
whenJust (Just a) f = f a

