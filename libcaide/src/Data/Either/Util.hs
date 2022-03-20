module Data.Either.Util(
      maybeToEither
    , mapLeft
    , orElse
    , whenLeft
) where

mapLeft :: (e1 -> e2) -> Either e1 a -> Either e2 a
mapLeft f = either (Left . f) Right

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) Right

orElse :: Either e a -> Either e a -> Either e a
orElse (Right a) _ = Right a
orElse (Left _) b = b

whenLeft :: Monad m => Either e a -> (e -> m ()) -> m ()
whenLeft e f = either f (const $ pure ()) e

