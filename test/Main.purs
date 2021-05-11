module Test.Main where

import Prelude

import Data.Array ((..))
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, decodeArrayBuffer, encodeArrayBuffer)
import Data.Either (Either(..))
import Data.Lens.Getter (view)
import Data.Lens.Iso (Iso')
import Data.Lens.Prism (review)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Domination.Capability.Random (runRandomM)
import Domination.Capability.WireCodec (class WireCodec, readWire, writeWire)
import Domination.Data.Cards as Cards
import Domination.Data.Game.Engine (makeAutoPlay) as Dom
import Domination.Data.Game as Game
import Domination.Data.Play (Play(..))
import Domination.Data.Wire.Game (_toWire) as Dom
import Effect (Effect)
import Test.QuickCheck (quickCheck')

main :: Effect Unit
main = do
  quickCheck' 1 `traverse_` examples

-- can't run this test outside of the browser due to FFI.js
-- using browser specific APIs (window) and libraries (LZString)
--  let wireWitchChoice = view _choiceToWire Cards.witchChoice
--  b <- runWireCodecM $ write_read wireWitchChoice
--  quickCheck' 1 b

-- properties

encode_decode
  :: forall a
  . EncodeArrayBuffer a
  => DecodeArrayBuffer a
  => DynamicByteLength a
  => Eq a
  => a
  -> Effect Boolean
encode_decode x = do
  ab <- encodeArrayBuffer x
  mbX <- decodeArrayBuffer ab
  pure case mbX of
    Nothing -> false
    Just x' -> x == x'

write_read
  :: forall m a
  . WireCodec m
  => EncodeArrayBuffer a
  => DynamicByteLength a
  => DecodeArrayBuffer a
  => Eq a
  => a
  -> m Boolean
write_read x = do
  eS <- writeWire x
  case eS of
    Left _ -> pure false
    Right s -> do
      eX' <- readWire s
      case eX' of
        Left _ -> pure false
        Right x' -> pure $ x' == x

game_wire_iso :: Int -> Boolean
game_wire_iso n = iso_prop Dom._toWire (Game.new n Cards.cardMap true)

-- examples

examples :: Array Boolean
examples = game_wire_iso <$> (1 .. 10)

exampleGame :: Effect Boolean
exampleGame = do
  let g0 = Game.new 1 Cards.cardMap true
  g1 <- runRandomM
    $ Dom.makeAutoPlay
      (NewGame { playerCount: 1, supply: Cards.cardMap, longGame: true })
      g0
  pure case g1 of
    Left _ -> false
    Right _ -> true

-- helpers

iso_prop
  :: forall a b
  . Eq a
  => (Iso' a b)
  -> a
  -> Boolean
iso_prop iso before = let
  mapped = view iso before
  after = review iso mapped
  in before == after

