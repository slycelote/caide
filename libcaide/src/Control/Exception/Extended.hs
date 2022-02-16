module Control.Exception.Extended(
      module Control.Exception
    , catchIf
    , handleIf
    , ignoring
) where

import Control.Exception

catchIf :: Exception e => (e -> Bool) -> IO a -> (e -> IO a) -> IO a
catchIf predicate action handler =
    catchJust (\e -> if predicate e then Just e else Nothing) action handler

handleIf :: Exception e => (e -> Bool) -> (e -> IO a) -> IO a -> IO a
handleIf predicate handler action =
    handleJust (\e -> if predicate e then Just e else Nothing) handler action

ignoring :: Exception e => IO () -> (e -> Bool) -> IO ()
action `ignoring` predicate = catchIf predicate action (const $ pure ())

