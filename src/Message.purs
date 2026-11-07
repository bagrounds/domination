--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Module providing data types, encoding/decoding functions, and rendering HTML for various game-related messages.
--|
--| ### Key Concepts
--| * RemoteMessage data type with various variants (ChatMessage, UsernameMessage, GameMessage, PlayMadeMessage)
--| * WireMessage data type with various variants (ChatWireMessage, UsernameWireMessage, GameWireMessage, PlayMadeWireMessage) and an Iso' transformation for conversion to/from
--| * Usage of Argonaut's genericDecodeJson, genericEncodeJson, deriveGeneric, eqRemoteMessage, showRemoteMessage, encodeJsonWireMessage, decodeJsonWireMessage, dynamicByteLengthWireMessage, and encodeArrayBuffeWireMessage

module Message where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, genericByteLength, genericPutArrayBuffer, genericReadArrayBuffer)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Lens.Getter (view)
import Data.Lens.Iso (Iso', iso)
import Data.Lens.Prism (review)
import Data.Tuple (Tuple(..))
import Domination.Data.Game (Game)
import Domination.Data.Play (Play)
import Domination.Data.Wire.Game (WireGame)
import Domination.Data.Wire.Game (_toWire) as Game
import Domination.Data.Wire.Play (WirePlay)
import Domination.Data.Wire.Play (_toWire) as Play
import Domination.Data.Wire.Int (WireInt)
import Domination.Data.Wire.Int as Int

type Envelope = { id :: String, message :: RemoteMessage }
type WireEnvelope = Tuple String WireMessage

wrapMessage :: String -> RemoteMessage -> Envelope
wrapMessage id message = { id, message }

data RemoteMessage
  = ChatMessage { username :: String, message :: String, chatNumber :: Int }
  | UsernameMessage { username :: String, id :: String }
  | GameMessage
    { i :: Int
    , state :: Game
    , playMade :: Maybe
      { play :: Play
      , playerIndex :: Int
      , state :: Game
      }
    }
  | PlayMadeMessage
    { play :: Play
    , playerIndex :: Int
    , state :: Game
    }
  | JoinMessage { clientId :: String, timestamp :: Int }
  | HeartbeatMessage { clientId :: String, timestamp :: Int }
  | LeaveMessage { clientId :: String, timestamp :: Int }

derive instance genericRemoteMessage :: Generic RemoteMessage _
derive instance eqRemoteMessage :: Eq RemoteMessage
instance showRemoteMessage :: Show RemoteMessage where
  show = genericShow
instance encodeJsonRemoteMessage :: EncodeJson RemoteMessage where
  encodeJson = genericEncodeJson
instance decodeJsonRemoteMessage :: DecodeJson RemoteMessage where
  decodeJson = genericDecodeJson

data WireMessage
  = ChatWireMessage String String WireInt
  | UsernameWireMessage (Tuple String String)
  | GameWireMessage
    (Tuple WireInt
    (Tuple WireGame
    (Maybe
    (Tuple WirePlay
    (Tuple WireInt WireGame)))))
  | PlayMadeWireMessage
    (Tuple WirePlay
    (Tuple WireInt WireGame))
  | JoinWireMessage String WireInt
  | HeartbeatWireMessage String WireInt
  | LeaveWireMessage String WireInt

_toWire :: Iso' RemoteMessage WireMessage
_toWire = iso to from where
  to = case _ of
    ChatMessage { username, message, chatNumber } ->
      ChatWireMessage username message (view Int._toWire chatNumber)
    UsernameMessage { username, id} ->
      UsernameWireMessage (Tuple username id)
    GameMessage { i, state, playMade } ->
      GameWireMessage
      $ Tuple (view Int._toWire i)
      $ Tuple (view Game._toWire state) (pmm <$> playMade)
    PlayMadeMessage x ->
      PlayMadeWireMessage $ pmm x
    JoinMessage { clientId, timestamp } ->
      JoinWireMessage clientId (view Int._toWire timestamp)
    HeartbeatMessage { clientId, timestamp } ->
      HeartbeatWireMessage clientId (view Int._toWire timestamp)
    LeaveMessage { clientId, timestamp } ->
      LeaveWireMessage clientId (view Int._toWire timestamp)
    where
      pmm { play, playerIndex, state } =
        Tuple (view Play._toWire play)
        $ Tuple
          (view Int._toWire playerIndex)
          (view Game._toWire state)
  from = case _ of
    ChatWireMessage username message chatNumber ->
      ChatMessage { username, message, chatNumber: review Int._toWire chatNumber }
    UsernameWireMessage (Tuple username id) ->
      UsernameMessage { username, id}
    GameWireMessage (Tuple i (Tuple state maybePlayMade)) ->
      GameMessage
        { i: review Int._toWire i
        , state: review Game._toWire state
        , playMade: pmm <$> maybePlayMade
        }
    PlayMadeWireMessage x ->
      PlayMadeMessage $ pmm x
    JoinWireMessage clientId timestamp ->
      JoinMessage { clientId, timestamp: review Int._toWire timestamp }
    HeartbeatWireMessage clientId timestamp ->
      HeartbeatMessage { clientId, timestamp: review Int._toWire timestamp }
    LeaveWireMessage clientId timestamp ->
      LeaveMessage { clientId, timestamp: review Int._toWire timestamp }
    where
      pmm (Tuple play (Tuple playerIndex state)) =
        { play: review Play._toWire play
        , playerIndex: review Int._toWire playerIndex
        , state: review Game._toWire state
        }

derive instance genericWireMessage :: Generic WireMessage _
derive instance eqWireMessage :: Eq WireMessage
instance showWireMessage :: Show WireMessage where
  show = genericShow
instance encodeJsonWireMessage :: EncodeJson WireMessage where
  encodeJson = genericEncodeJson
instance decodeJsonWireMessage :: DecodeJson WireMessage where
  decodeJson = genericDecodeJson
instance dynamicByteLengthWireMessage
  :: DynamicByteLength WireMessage where
  byteLength = genericByteLength
instance encodeArrayBuffeWireMessage
  :: EncodeArrayBuffer WireMessage where
  putArrayBuffer = genericPutArrayBuffer
instance decodeArrayBuffeWireMessage
  :: DecodeArrayBuffer WireMessage where
  readArrayBuffer = genericReadArrayBuffer
