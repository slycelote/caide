{-# LANGUAGE CPP, FlexibleContexts, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Caide.Xml(
      isTag
    , goToChild
    , goToDocRoot
    , removeChildren
    , forEachChild
    , insertLastChild
    , mkElem
    , mkText
    , modifyFromJust
    , getTextContent
    , changeAttr
    , getAttr
    , hasAttrEqualTo
    , hasAttr
    , showXml
    , maybeDo
    , XmlState
    , runXmlTransformation
) where

#ifndef AMP
import Control.Applicative (Applicative)
#endif
import Control.Monad.Except (ExceptT, runExceptT, throwError, MonadError)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State.Strict (get, put, modify, MonadState, StateT, runStateT, gets)
import Data.Char (toLower)
import Data.List (find)
import Data.Maybe (isJust, fromJust)
import qualified Data.Text as T

import Text.XML.Light (Content(..), QName(..), Element(..), Attr(..), blank_element, unqual, showContent)
import Text.XML.Light.Cursor
import Text.XML.Light.Proc (strContent)
import Text.XML.Light.Types (CData(..), CDataKind(..))



-- | Executes one of the actions, depending on whether state transition function succeeds.
maybeDo :: MonadState s m => (s -> Maybe s) -> m a -> m a -> m a
maybeDo f successAction defaultAction = do
    s <- get
    case f s of
        Nothing -> defaultAction
        Just s' -> put s' >> successAction

--modifyFromJust f = modify (fromJust . f)
modifyFromJust ::  (MonadError T.Text m, MonadState s m) =>
                    T.Text -> (s -> Maybe s) -> m ()
modifyFromJust messageIfNothing f = do
    s <- gets f
    case s of
        Nothing -> throwError messageIfNothing
        Just s' -> put s'

equalIgnoreCase :: String -> String -> Bool
equalIgnoreCase s1 s2 = map toLower s1 == map toLower s2

-- | Checks whether current position points at specific tag name
isTag :: String -> Cursor -> Bool
isTag name (Cur (Elem e) _ _ _) = equalIgnoreCase name $ qName(elName e)
isTag _ _ = False

getAttr :: String -> Cursor -> Maybe String
getAttr key (Cur (Elem el) _ _ _) = case find (attribHasName key) (elAttribs el) of
    Just att -> Just $ attrVal att
    _        -> Nothing
getAttr _ _ = Nothing

hasAttrEqualTo :: String -> String -> Cursor -> Bool
hasAttrEqualTo key value cur = case getAttr key cur of
    Just attrValue -> equalIgnoreCase value attrValue
    _        -> False

hasAttr :: String -> Cursor -> Bool
hasAttr key (Cur (Elem el) _ _ _) = isJust $ find (attribHasName key) (elAttribs el)
hasAttr _ _ = False

mkAttr :: String -> String -> Attr
mkAttr = Attr . unqual

mkElem :: String -> [(String, String)] -> Element
mkElem name attrs = blank_element {elName = unqual name, elAttribs = map (uncurry mkAttr) attrs}

mkText :: String -> Content
mkText text = Text CData{cdVerbatim = CDataRaw, cdData = text, cdLine = Nothing}

-- | Removes children of current element that satisfy the condition.
removeChildren ::  MonadState Cursor m => (Cursor -> Bool) -> m ()
removeChildren predicate = do
    c <- gets (findChild predicate)
    case c of
        Nothing    -> return ()
        Just child -> do
            put $ fromJust $ removeGoUp child
            removeChildren predicate

-- | Modify each child that satisfies the predicate. The action is required to return to the child in the end.
forEachChild ::  (MonadError T.Text m, MonadState Cursor m) =>
                 (Cursor -> Bool) -> m t -> m [t]
forEachChild predicate action = do
    c <- gets (findChild predicate)
    case c of
        Nothing    -> return []
        Just child -> put child >> go []
  where
    go results = do
        ret <- action
        cur <- get
        case findRight predicate cur of
            Nothing -> modifyFromJust "Root doesn't have a parent" parent >> return (reverse (ret:results))
            Just c  -> put c >> go (ret:results)

-- | Follow a series of children links to particular tags
goToChild ::  (MonadError T.Text m, MonadState Cursor m) => [String] -> m ()
goToChild [] = return ()
goToChild (x:xs) = maybeDo (findChild (isTag x))
                           (goToChild xs)
                           (throwError . T.concat $ ["No ", T.pack x, " child found"])

-- | Go to the root of the first tree of the cursor's forest
goToDocRoot :: MonadState Cursor m => m ()
goToDocRoot = modify (go . root) where
    go c = case left c of
               Nothing -> c
               Just c' -> go c'

showXml :: Cursor -> String
showXml = dropWhile (/= '<') . unlines . map showContent . toForest

-- | Insert the child at the end of the children list and move to the child
insertLastChild :: (MonadError T.Text m, MonadState Cursor m) =>
                    Content -> m ()
insertLastChild cont = do
    let insert (Elem el) = Elem (el {elContent = elContent el ++ [cont]})
        insert _ = error "Should not happen"
    cur <- gets current
    case cur of
        Elem _ -> do
            modify $ modifyContent insert
            modifyFromJust "insertLastChild: couldn't move to inserted child" lastChild
        _      -> throwError "Current node is node an Elem"

attribHasName :: String -> Attr -> Bool
attribHasName name = (== unqual name) . attrKey

changeAttr :: MonadState Cursor m => String -> String -> m Bool
changeAttr key value = do
    noChange <- gets (hasAttrEqualTo key value)
    if noChange
    then return False
    else changeAttr' key value >> return True

changeAttr' :: MonadState Cursor m => String -> String -> m ()
changeAttr' name value = modify $ modifyContent change
    where change (Elem el) = Elem (el {elAttribs = changeAttribs (elAttribs el)})
          change _ = error "Only Element can have attributes"
          changeAttribs attrs = mkAttr name value : filter (not . attribHasName name) attrs

getTextContent :: Cursor -> Maybe String
getTextContent = getTextContent' . current

getTextContent' :: Content -> Maybe String
getTextContent' (Elem t) = Just $ strContent t
getTextContent' _ = Nothing

newtype XmlState a = XmlState { runXmlState :: StateT Cursor (ExceptT T.Text Identity) a }
    deriving (Applicative, Functor, Monad, MonadState Cursor, MonadError T.Text)

runXmlTransformation ::  XmlState a -> Cursor -> Either T.Text (a, Cursor)
runXmlTransformation x = runIdentity . runExceptT . runStateT (runXmlState x)

