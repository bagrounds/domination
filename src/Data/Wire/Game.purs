--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defines types, conversions, and encodings for representing a game state in wire format.
--|
--| ### Key Concepts
--| * Data transformation between WireGame and Game using an Iso
--| * Generic encoding/decoding for WireGame using Argonaut's generic functions
--| * Lens-based getters and setters for accessing WireGame fields
module Domination.Data.Wire.Game where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Array.NonEmpty (fromNonEmpty, toNonEmpty)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, genericByteLength, genericPutArrayBuffer, genericReadArrayBuffer)
import Data.Generic.Rep (class Generic)
import Data.Lens.Getter (view)
import Data.Lens.Iso (Iso', iso)
import Data.Lens.Prism (review)
import Data.Lens.Setter ((%~))
import Data.Maybe (Maybe)
import Data.NonEmpty (NonEmpty)
import Data.Show.Generic (genericShow)
import Domination.Data.Game (Game, _players, _result, _supply, _trash, _turn)
import Domination.Data.Phase (Phase)
import Domination.Data.Player (WirePlayer)
import Domination.Data.Player as Player
import Domination.Data.Wire.Card as Card
import Domination.Data.Wire.Int (WireInt)
import Domination.Data.Wire.Int as Int
import Domination.Data.Wire.Result (WireResult)
import Domination.Data.Wire.Result as Result
import Domination.Data.Wire.Stack (WireStack)
import Domination.Data.Wire.Supply as Supply
import Util ((<$>~))

data WireGame =
  WireGame
    Phase
    (NonEmpty Array WirePlayer)
    (Array WireStack)
    (Array WireInt)
    WireInt
    (Maybe WireResult)
    Boolean

fromWire :: WireGame -> Game
fromWire = review _toWire

_toWire :: Iso' Game WireGame
_toWire = iso to from where
  to = (_turn %~ view Int._toWire)
    >>> (_players %~ toNonEmpty)
    >>> (_players <$>~ view Player._toWire)
    >>> (_supply %~ view Supply._toWire)
    >>> (_trash <$>~ view Card._toWire)
    >>> (_result <$>~ view Result._toWire)
    >>> to'
  from = from'
    >>> (_turn %~ review Int._toWire)
    >>> (_players %~ fromNonEmpty)
    >>> (_players <$>~ review Player._toWire)
    >>> (_supply %~ review Supply._toWire)
    >>> (_trash <$>~ (review Card._toWire))
    >>> (_result <$>~ review Result._toWire)
  to' { phase, players, result, supply, trash, turn, longGame } =
    WireGame phase players supply trash turn result longGame
  from' (WireGame phase players supply trash turn result longGame) =
    { phase, players, result, supply, trash, turn, longGame }

derive instance genericWireGame
  :: Generic WireGame _

derive instance eqWireGame :: Eq WireGame

instance showWireGame :: Show WireGame where
  show wireChoice = genericShow wireChoice

instance encodeJsonWireGame
  :: EncodeJson WireGame where
  encodeJson a = genericEncodeJson a

instance decodeJsonWireGame
  :: DecodeJson WireGame where
  decodeJson a = genericDecodeJson a

instance dynamicByteLengthWireGame
  :: DynamicByteLength WireGame where
  byteLength x = genericByteLength x

instance encodeArrayBuffeWireGame
  :: EncodeArrayBuffer WireGame where
  putArrayBuffer x = genericPutArrayBuffer x

instance decodeArrayBuffeWireGame
  :: DecodeArrayBuffer WireGame where
  readArrayBuffer x = genericReadArrayBuffer x
