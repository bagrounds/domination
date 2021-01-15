module Util where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.State.Class (class MonadState, get, put)
import Data.Argonaut (stringify)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (deleteAt, drop, filter, length, nub, take, zip, (!!), (:))
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Foldable (any, elem, notElem)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens.Getter (view)
import Data.Lens.Lens (Lens')
import Data.Lens.Setter (Setter', over, set, subOver)
import Data.Lens.Traversal (traverseOf)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)

class RenderText a where
  renderText :: a -> String

dropIndex :: forall a. Int -> Array a -> Array a
dropIndex i xs = take i xs <> drop (i + 1) xs

withIndices :: forall a. Array a -> Array (Tuple Int a)
withIndices xs = zip (indices xs) xs

dropIndices :: forall a. Array Int -> Array a -> Maybe (Array a)
dropIndices is xs =
  if length is > length xs
  || length (nub is) /= length is
  || (_ < 0) `any` is
  || (_ >= length xs) `any` is
  then Nothing
  else Just $ snd <$>
  (fst >>> flip notElem is) `filter` withIndices xs

takeIndices :: forall a. Array Int -> Array a -> Maybe (Array a)
takeIndices is xs =
  if length is > length xs
  || length (nub is) /= length is
  || (_ < 0) `any` is
  || (_ >= length xs) `any` is
  then Nothing
  else Just $ snd <$>
  (fst >>> flip elem is) `filter` withIndices xs

indices :: forall a. Array a -> Array Int
indices xs = fst <$> mapWithIndex Tuple xs

justIf :: forall a. (a -> Boolean) -> a -> Maybe a
justIf f x = if f x then Just x else Nothing

assert :: forall e m a. MonadError e m => e -> (a -> Boolean) -> a -> m a
assert e p x = if p x then pure x else throwError e

fromJust :: forall e m a. MonadError e m => e -> Maybe a -> m a
fromJust e = case _ of
  Nothing -> throwError e
  Just a -> pure a

type ArrayLens' s a = Lens' s (Array a)

dropNthOf :: forall s a. Int -> ArrayLens' s a -> s -> Maybe s
dropNthOf n lens = traverseOf lens $ deleteAt n

dropFirstOf :: forall s a. ArrayLens' s a -> s -> Maybe s
dropFirstOf = dropNthOf 0

prependOver :: forall s a. ArrayLens' s a -> a -> s -> s
prependOver lens x = over lens (x : _)

decOver :: forall s. Setter' s Int -> s -> s
decOver = flip subOver 1

nthOf :: forall s a. Int -> ArrayLens' s a -> s -> Maybe a
nthOf i lens = view lens >>> (_ !! i)

firstOf :: forall s a. ArrayLens' s a -> s -> Maybe a
firstOf = nthOf 0

moveOne :: forall s a . ArrayLens' s a -> ArrayLens' s a -> s -> Maybe s
moveOne lens1 lens2 s = do
  x <- firstOf lens1 s
  let s' = prependOver lens2 x s
  dropFirstOf lens1 s'

moveAll :: forall s a . ArrayLens' s a -> ArrayLens' s a -> s -> s
moveAll lens1 lens2 s =
  let from = view lens1 s in
  let s' = over lens2 (from <> _) s in
  set lens1 [] s'

modifyM_ :: forall s m. MonadState s m => (s -> m s) -> m Unit
modifyM_ f = get >>= f >>= put

readJson :: forall a. DecodeJson a => String -> Either String a
readJson = lmap show <<< decodeJson <=< jsonParser

writeJson :: forall a. EncodeJson a => a -> String
writeJson = stringify <<< encodeJson

