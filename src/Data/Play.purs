module Domination.Data.Play where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, genericByteLength, genericPutArrayBuffer, genericReadArrayBuffer)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens.Getter (view)
import Data.Lens.Internal.Wander (wander)
import Data.Lens.Iso (Iso', iso)
import Data.Lens.Lens (Lens')
import Data.Lens.Prism (review)
import Data.Lens.Record (prop)
import Data.Lens.Traversal (Traversal', traverseOf)
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Domination.Data.Card (Card)
import Domination.Data.Cards as Cards
import Domination.Data.Choice (Choice)
import Domination.Data.Reaction (Reaction)
import Domination.Data.Wire.Choice (WireChoice)
import Domination.Data.Wire.Choice as Choice
import Domination.Data.WireInt (WireInt, _WireInt)

data Play
  = NewGame
    { playerCount :: Int
    , supply :: Array Card
    , longGame :: Boolean
    }
  | EndPhase { playerIndex :: Int }
  | PlayCard { playerIndex :: Int, cardIndex :: Int }
  | Purchase { playerIndex :: Int,  stackIndex :: Int }
  | ResolveChoice { playerIndex :: Int, choice :: Choice }
  | React { playerIndex :: Int, reaction :: Maybe Reaction }

_toWire :: Iso' Play WirePlay
_toWire = iso to from where
  to = case _ of
    NewGame { playerCount, supply, longGame } ->
      WireNewGame
        (view _WireInt playerCount)
        (view Cards._toWire <$> supply)
        longGame
    EndPhase { playerIndex } ->
      WireEndPhase $ view _WireInt playerIndex
    PlayCard { playerIndex, cardIndex } ->
      WirePlayCard $ Tuple
        (view _WireInt playerIndex)
        (view _WireInt cardIndex)
    Purchase { playerIndex,  stackIndex } ->
      WirePurchase $ Tuple
        (view _WireInt playerIndex)
        (view _WireInt stackIndex)
    ResolveChoice { playerIndex, choice } ->
      WireResolveChoice $ Tuple
        (view _WireInt playerIndex)
        (view Choice._toWire choice)
    React { playerIndex, reaction } ->
      WireReact $ Tuple (view _WireInt playerIndex) reaction
  from = case _ of
    WireNewGame playerCount supply longGame ->
      NewGame
        { playerCount: review _WireInt playerCount
        , supply: review Cards._toWire <$> supply
        , longGame
        }
    WireEndPhase playerIndex ->
      EndPhase { playerIndex: review _WireInt playerIndex }
    WirePlayCard (Tuple playerIndex cardIndex) ->
      PlayCard
        { playerIndex: review _WireInt playerIndex
        , cardIndex: review _WireInt cardIndex
        }
    WirePurchase (Tuple playerIndex stackIndex) ->
      Purchase
        { playerIndex: review _WireInt playerIndex
        , stackIndex: review _WireInt stackIndex
        }
    WireResolveChoice (Tuple playerIndex choice) ->
      ResolveChoice
        { playerIndex: review _WireInt playerIndex
        , choice: review Choice._toWire choice
        }
    WireReact (Tuple playerIndex reaction) ->
      React
        { playerIndex: review _WireInt playerIndex
        , reaction
        }

_playerIndex' :: forall r. Lens' { playerIndex :: Int | r } Int
_playerIndex' = prop (SProxy :: SProxy "playerIndex")

_playerIndex :: Traversal' Play Int
_playerIndex = wander \f s -> case s of
  NewGame _ -> pure s
  EndPhase x -> EndPhase <$> traverseOf _playerIndex' f x
  PlayCard x -> PlayCard <$> traverseOf _playerIndex' f x
  Purchase x -> Purchase <$> traverseOf _playerIndex' f x
  ResolveChoice x -> ResolveChoice <$> traverseOf _playerIndex' f x
  React x -> React <$> traverseOf _playerIndex' f x

derive instance genericPlay :: Generic Play _
derive instance eqPlay :: Eq Play
instance encodeJsonPlay :: EncodeJson Play where
  encodeJson = genericEncodeJson
instance decodeJsonPlay :: DecodeJson Play where
  decodeJson = genericDecodeJson
instance showPlay :: Show Play where
  show = genericShow

data WirePlay
  = WireNewGame WireInt (Array WireInt) Boolean
  | WireEndPhase WireInt
  | WirePlayCard (Tuple WireInt WireInt)
  | WirePurchase (Tuple WireInt WireInt)
  | WireResolveChoice (Tuple WireInt WireChoice)
  | WireReact (Tuple WireInt (Maybe Reaction))

derive instance genericWirePlay :: Generic WirePlay _
derive instance eqWirePlay :: Eq WirePlay
instance encodeJsonWirePlay :: EncodeJson WirePlay where
  encodeJson = genericEncodeJson
instance decodeJsonWirePlay :: DecodeJson WirePlay where
  decodeJson = genericDecodeJson
instance showWirePlay :: Show WirePlay where
  show = genericShow
instance dynamicByteLengthWirePlay :: DynamicByteLength WirePlay where
  byteLength = genericByteLength
instance encodeArrayBuffeWirePlay :: EncodeArrayBuffer WirePlay where
  putArrayBuffer = genericPutArrayBuffer
instance decodeArrayBuffeWirePlay :: DecodeArrayBuffer WirePlay where
  readArrayBuffer = genericReadArrayBuffer

