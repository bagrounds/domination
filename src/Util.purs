module Util where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.State.Class (class MonadState, get, put)
import Data.Argonaut (stringify)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (deleteAt, drop, filter, length, nub, take, (!!), (:))
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Foldable (any, elem, notElem)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Lens.Getter (view, (^.))
import Data.Lens.Lens (Lens, Lens', lens')
import Data.Lens.Lens.Tuple (_1, _2)
import Data.Lens.Prism (Review, review)
import Data.Lens.Setter (Setter', over, set, subOver, (.~))
import Data.Lens.Traversal (traverseOf)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)

class RenderText a where
  renderText :: a -> String

dropIndex :: forall a. Int -> Array a -> Array a
dropIndex i xs = take i xs <> drop (i + 1) xs

withIndices :: forall a f. FunctorWithIndex Int f => f a -> f (Tuple Int a)
withIndices = mapWithIndex Tuple

dropIndices
  :: forall m a
  . MonadError String m
  => Array Int
  -> Array a
  -> m (Array a)
dropIndices is xs =  fromJust "failed to drop indices" $
  if length is > length xs
  || length (nub is) /= length is
  || (_ < 0) `any` is
  || (_ >= length xs) `any` is
  then Nothing
  else Just $ snd <$>
  (fst >>> flip notElem is) `filter` withIndices xs

takeIndices
  :: forall m a
  . MonadError String m
  => Array Int
  -> Array a
  -> m (Array a)
takeIndices is xs = fromJust "failed to drop indices" $
  if length is > length xs
  || length (nub is) /= length is
  || (_ < 0) `any` is
  || (_ >= length xs) `any` is
  then Nothing
  else Just $ snd <$>
  (fst >>> flip elem is) `filter` withIndices xs

indices :: forall f a. FunctorWithIndex Int f => f a -> f Int
indices xs = fst <$> mapWithIndex Tuple xs

justIf :: forall a. (a -> Boolean) -> a -> Maybe a
justIf f x = if f x then Just x else Nothing

assert
  :: forall e m a
  . MonadError e m
  => (a -> Boolean)
  -> e
  -> a
  -> m a
assert p e x = if p x then pure x else throwError e

assert_
  :: forall e m a
  . MonadError e m
  => (a -> Boolean)
  -> e
  -> a
  -> m Unit
assert_ p e x = if p x then pure unit else throwError e

forget :: forall a. a -> Unit
forget _ = unit

fromJust :: forall e m a. MonadError e m => e -> Maybe a -> m a
fromJust e = case _ of
  Nothing -> throwError e
  Just a -> pure a

type ArrayLens' s a = Lens' s (Array a)

dropNthOf :: forall s a. Int -> ArrayLens' s a -> s -> Maybe s
dropNthOf n lens = traverseOf lens $ deleteAt n

dropFirstOf :: forall s a. ArrayLens' s a -> s -> Maybe s
dropFirstOf = dropNthOf 0

reviewOn :: forall s t a b. b -> Review s t a b -> t
reviewOn = flip review

infixl 8 reviewOn as .^

prependOver :: forall s a. ArrayLens' s a -> a -> s -> s
prependOver lens x = over lens (x : _)

infixr 4 prependOver as :~

mapOver
  :: forall f s t a b
  . Functor f
  => Lens s t (f a) (f b)
  -> (a -> b)
  -> s
  -> t
mapOver lens f = over lens (map f)

infixr 4 mapOver as <$>~

preAppendOver :: forall s a. ArrayLens' s a -> Array a -> s -> s
preAppendOver lens xs = over lens (xs <> _)

decOver :: forall s n. Ring n => Setter' s n -> s -> s
decOver = flip subOver one

nthOf :: forall s a. Int -> ArrayLens' s a -> s -> Maybe a
nthOf i lens = view lens >>> (_ !! i)

firstOf :: forall s a. ArrayLens' s a -> s -> Maybe a
firstOf = nthOf 0

moveOne
  :: forall s a
  . ArrayLens' s a
  -> ArrayLens' s a
  -> s
  -> Maybe s
moveOne lens1 lens2 s = do
  x <- firstOf lens1 s
  let s' = prependOver lens2 x s
  dropFirstOf lens1 s'

moveUpTo
  :: forall s a
  . Int
  -> ArrayLens' s a
  -> ArrayLens' s a
  -> s
  -> s
moveUpTo n lens1 lens2 s =
  let
    from = view lens1 s
    s' = over lens2 (take n from <> _) s
  in
  set lens1 (drop n from) s'

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

_tuple
  :: forall s a b
  . (Lens' s a)
  -> (Lens' s b)
  -> Lens' s (Tuple a b)
_tuple _l1 _l2 = lens' f
  where
    f s = Tuple (Tuple (s ^. _l1) (s ^. _l2)) g
      where
        g (Tuple a b) = ((_l1 .~ a) <<< (_l2 .~ b)) s

infixr 4 _tuple as ~&~

_22 :: forall a b c. Lens' (Tuple a (Tuple b c)) c
_22 = _2 <<< _2

_11 :: forall a b c. Lens' (Tuple (Tuple a b) c) a
_11 = _1 <<< _1

_21 :: forall a b c. Lens' (Tuple a (Tuple b c)) b
_21 = _2 <<< _1

_12 :: forall a b c. Lens' (Tuple (Tuple a b) c) b
_12 = _1 <<< _2

_composePredicate
  :: forall a b
  . Lens' a b
  -> (b -> Boolean)
  -> a -> Boolean
_composePredicate _l p = p <<< view _l

infixr 8 _composePredicate as <<~

