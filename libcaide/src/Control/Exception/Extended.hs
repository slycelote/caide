module Control.Exception.Extended(
      module Control.Exception
    , catchIf
    , handleIf
    , ignoring
) where

import Control.Exception

catchIf :: Exception e => (e -> Bool) -> IO a -> (e -> IO a) -> IO a
catchIf predicate = catchJust (\e -> if predicate e then Just e else Nothing)

handleIf :: Exception e => (e -> Bool) -> (e -> IO a) -> IO a -> IO a
handleIf predicate = handleJust (\e -> if predicate e then Just e else Nothing)

ignoring :: Exception e => IO () -> (e -> Bool) -> IO ()
action `ignoring` predicate = catchIf predicate action (const $ pure ())

