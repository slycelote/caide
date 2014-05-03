module Caide.Xml (
      isTag
    , goToChild
    , goToDocRoot
    , removeChildren
    , insertLastChild
    , mkElem
    , modifyFromJust
    , changeAttr
    , hasAttr
    , showXml
) where

import Control.Monad.State.Strict (get, put, modify, MonadState, State, runState, gets)
import Data.Char (toLower)
import Data.List (find)
import Data.Maybe (fromJust)

import Text.XML.Light (Content(..), QName(..), Element(..), Attr(..), blank_element, unqual, showContent)
import Text.XML.Light.Cursor


-- | Executes one of the actions, depending on whether state transition function succeeds.
maybeDo :: MonadState s m => (s -> Maybe s) -> m a -> m a -> m a
maybeDo f successAction defaultAction = do
    s <- get
    case f s of
        Nothing -> defaultAction
        Just s' -> put s' >> successAction

modifyFromJust :: MonadState s m => (s -> Maybe s) -> m ()
modifyFromJust f = modify (fromJust . f)

-- | Tries to execute the action and undoes the effect if it was unsuccessful.
tryDo :: MonadState s m => State s Bool -> m Bool
tryDo action = do
    s <- get
    case runState action s of
        (True, s') -> put s' >> return True
        _          -> return False

-- | Checks whether current position points at specific tag name
isTag :: String -> Cursor -> Bool
isTag name (Cur (Elem e) _ _ _)
    | map toLower(qName(elName e)) == map toLower name  = True
isTag _ _ = False

hasAttr :: String -> String -> Cursor -> Bool
hasAttr key value (Cur (Elem el) _ _ _) = case find (attribHasName key) (elAttribs el) of
    Just att -> attrVal att == value
    _        -> False
hasAttr _ _ _ = False

mkAttr :: String -> String -> Attr
mkAttr = Attr . unqual

mkElem :: String -> [(String, String)] -> Element
mkElem name attrs = blank_element {elName = unqual name, elAttribs = map (uncurry mkAttr) attrs}


-- | Removes children of current element that satisfy the condition.
removeChildren :: (Cursor -> Bool) -> State Cursor ()
removeChildren predicate = maybeDo firstChild go $ return ()
    where go = maybeDo removeOrRight go $ modify (fromJust . parent)
          removeOrRight c | predicate c = removeGoRight c
                          | otherwise   = right c

goToChild' :: [String] -> State Cursor Bool
goToChild' [] = return True
goToChild' (x:xs) = maybeDo (findChild (isTag x)) (goToChild' xs) (return False)

-- | Follows a series of children links to particular tags
goToChild :: [String] -> State Cursor Bool
goToChild path = tryDo (goToChild' path)

-- | Go to the root of the first tree of the cursor's forest
goToDocRoot :: State Cursor ()
goToDocRoot = modify (go . root) where
    go c = case left c of
                Nothing -> c
                Just c' -> go c'

showXml :: Cursor -> String
showXml = unlines . map showContent . toForest

-- | Insert the child at the end of the children list and moves to the child
insertLastChild :: Content -> State Cursor Bool
insertLastChild cont = do
    let insert (Elem el) = Elem (el {elContent = elContent el ++ [cont]})
        insert _ = error "Should not happen"
    cur <- gets current
    case cur of
        Elem _ -> do
            modify $ modifyContent insert
            modifyFromJust lastChild
            return True
        _      -> return False

attribHasName :: String -> Attr -> Bool
attribHasName name = (== unqual name) . attrKey

changeAttr :: String -> String -> State Cursor ()
changeAttr name value = modify $ modifyContent change
    where change (Elem el) = Elem (el {elAttribs = changeAttribs (elAttribs el)})
          change _ = error "Only Element can have attributes"
          changeAttribs attrs = mkAttr name value : filter (not . attribHasName name)  attrs
