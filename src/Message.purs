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
import Data.Show.Generic (genericShow)
import Data.Lens.Fold (preview, (^?))
import Data.Lens.Getter (view)
import Data.Lens.Iso (Iso', iso)
import Data.Lens.Prism (review)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Domination.Data.Card as Card
import Domination.Data.Game (Game)
import Domination.Data.Game (_player, _stack) as Game
import Domination.Data.Play (Play(..))
import Domination.Data.Player as Player
import Domination.Data.Reaction (Reaction(..))
import Domination.Data.Wire.Game (WireGame)
import Domination.Data.Wire.Game (_toWire) as Game
import Domination.Data.Wire.Play (WirePlay)
import Domination.Data.Wire.Play (_toWire) as Play
import Domination.Data.Wire.Int (WireInt)
import Domination.Data.Wire.Int as Int
import Domination.UI.RenderText (renderTextInContext)
import Halogen.HTML (ClassName(..), HTML)
import Halogen.HTML (text) as HH
import Halogen.HTML.Elements (div, span, span_) as HH
import Halogen.HTML.Properties (class_) as HH

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
  | JoinMessage { clientId :: String }
  | HeartbeatMessage { clientId :: String, timestamp :: Int }
  | LeaveMessage { clientId :: String }

data LocalMessage
  = SeenMessage String
  | ConnectionsMessage Int

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
  | JoinWireMessage String
  | HeartbeatWireMessage String WireInt
  | LeaveWireMessage String

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
    JoinMessage { clientId } ->
      JoinWireMessage clientId
    HeartbeatMessage { clientId, timestamp } ->
      HeartbeatWireMessage clientId (view Int._toWire timestamp)
    LeaveMessage { clientId } ->
      LeaveWireMessage clientId
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
    JoinWireMessage clientId ->
      JoinMessage { clientId }
    HeartbeatWireMessage clientId timestamp ->
      HeartbeatMessage { clientId, timestamp: review Int._toWire timestamp }
    LeaveWireMessage clientId ->
      LeaveMessage { clientId }
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

renderHtml :: forall w i. RemoteMessage -> HTML w i
renderHtml (JoinMessage { clientId }) = HH.div
  [ HH.class_ $ ClassName "join-message" ]
  [ HH.text $ "(" <> clientId <> " has joined the game)" ]
renderHtml (HeartbeatMessage { clientId }) = HH.div
  [ HH.class_ $ ClassName "heartbeat-message" ]
  [ HH.text $ "(" <> clientId <> " is alive " ]
renderHtml (LeaveMessage { clientId }) = HH.div
  [ HH.class_ $ ClassName "leave-message" ]
  [ HH.text $ "(" <> clientId <> " has left the game)" ]
renderHtml (ChatMessage { username, message }) =
  HH.div
    [ HH.class_ $ ClassName "chat-message" ]
    [ HH.span [ HH.class_ $ ClassName "username" ] [ HH.text username ]
    , HH.text ": "
    , HH.span [ HH.class_ $ ClassName "message" ] [ HH.text message ]
    ]
renderHtml (GameMessage _) =
  HH.div
    [ HH.class_ $ ClassName "game-state-message" ]
    [ HH.div [ HH.class_ $ ClassName "game-state" ] [ HH.text "(Game Data Received)" ]
    ]
renderHtml (UsernameMessage { username, id }) =
  HH.div
    [ HH.class_ $ ClassName "username-message" ]
    [ HH.text "("
    , HH.span [ HH.class_ $ ClassName "username" ] [ HH.text $ show username ]
    , HH.text " has ID: "
    , HH.span [ HH.class_ $ ClassName "username" ] [ HH.text $ show id ]
    , HH.text ")"
    ]
renderHtml (PlayMadeMessage { play, playerIndex: player, state }) =
  case play' of
    Nothing -> HH.span [] []
    Just text -> HH.div
      [ HH.class_ $ ClassName "play-made-message" ]
      [ HH.text $ "Player " <> show (player + 1) <> ": "
      , HH.span
        [ HH.class_ $ ClassName "play-made" ]
        [ text ]
      ]
  where
      play' :: Maybe (HTML w i)
      play' = case play of
        NewGame { playerCount } -> Just $
          HH.span_
            [ HH.text $ "created a new "
              <> show playerCount <> " player game"
            ]
        EndPhase _ -> Nothing
        PlayCard { playerIndex, cardIndex } -> Just $
          HH.text $ "played: "
            <> getPlayerCardName playerIndex state cardIndex
        Purchase { stackIndex } -> Just $
          HH.text $ "purchased: " <> text
          where
            text = case preview (Game._stack stackIndex) state of
              Nothing -> "???"
              Just { card } -> card.name
        ResolveChoice { playerIndex, choice } ->
          Just $ renderTextInContext playerIndex state choice
        React { reaction } -> Just $
          HH.text $ case reaction of
            Nothing -> "did not react"
            Just BlockAttack -> "blocked an attack"

getPlayerCardName :: Int -> Game -> Int -> String
getPlayerCardName playerIndex state cardIndex = fromMaybe "???"
  $ state ^? Game._player playerIndex
    <<< Player._cardInHand cardIndex
    <<< Card._name
