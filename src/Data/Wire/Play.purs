module Domination.Data.Wire.Play where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, genericByteLength, genericPutArrayBuffer, genericReadArrayBuffer)
import Data.Generic.Rep (class Generic)
import Data.Lens.Getter (view, (^.))
import Data.Lens.Iso (Iso', iso)
import Data.Lens.Prism (review)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Domination.Data.Play (Play(..))
import Domination.Data.Wire.Choice (WireChoice)
import Domination.Data.Wire.Choice as Choice
import Domination.Data.Wire.Int (WireInt)
import Domination.Data.Wire.Int as Int
import Domination.Data.Wire.Reaction (WireReaction)
import Domination.Data.Wire.Reaction as Reaction
import Util ((.^))

data WirePlay
  = WireEndPhase WireInt
  | WirePlayCard (Tuple WireInt WireInt)
  | WirePurchase (Tuple WireInt WireInt)
  | WireResolveChoice (Tuple WireInt WireChoice)
  | WireReact (Tuple WireInt (Maybe WireReaction))
  | WireDoneReacting WireInt

_toWire :: Iso' Play WirePlay
_toWire = iso to from where
  to = case _ of
    EndPhase { playerIndex } ->
      WireEndPhase $ view Int._toWire playerIndex
    PlayCard { playerIndex, cardIndex } ->
      WirePlayCard $ Tuple
        (view Int._toWire playerIndex)
        (view Int._toWire cardIndex)
    Purchase { playerIndex,  stackIndex } ->
      WirePurchase $ Tuple
        (view Int._toWire playerIndex)
        (view Int._toWire stackIndex)
    ResolveChoice { playerIndex, choice } ->
      WireResolveChoice $ Tuple
        (view Int._toWire playerIndex)
        (view Choice._toWire choice)
    React { playerIndex, reaction } ->
      WireReact
        $ Tuple
          (view Int._toWire playerIndex)
          (view Reaction._toWire <$> reaction)
    DoneReacting { playerIndex } ->
      WireDoneReacting $ playerIndex ^. Int._toWire

  from = case _ of
    WireEndPhase playerIndex ->
      EndPhase { playerIndex: review Int._toWire playerIndex }
    WirePlayCard (Tuple playerIndex cardIndex) ->
      PlayCard
        { playerIndex: review Int._toWire playerIndex
        , cardIndex: review Int._toWire cardIndex
        }
    WirePurchase (Tuple playerIndex stackIndex) ->
      Purchase
        { playerIndex: review Int._toWire playerIndex
        , stackIndex: review Int._toWire stackIndex
        }
    WireResolveChoice (Tuple playerIndex choice) ->
      ResolveChoice
        { playerIndex: review Int._toWire playerIndex
        , choice: review Choice._toWire choice
        }
    WireReact (Tuple playerIndex reaction) ->
      React
        { playerIndex: review Int._toWire playerIndex
        , reaction: review Reaction._toWire <$> reaction
        }
    WireDoneReacting playerIndex ->
      DoneReacting { playerIndex: playerIndex .^ Int._toWire }

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

