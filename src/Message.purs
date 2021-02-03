module Message where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Array (intercalate)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, genericByteLength, genericPutArrayBuffer, genericReadArrayBuffer)
import Data.ArrayBuffer.Class.Types (Int8(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens.Fold (preview, (^?))
import Data.Lens.Getter (view)
import Data.Lens.Iso (Iso', iso)
import Data.Lens.Prism (review)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Domination.Data.Card as Card
import Domination.Data.GameState (GameState, WireGameState)
import Domination.Data.GameState as GameState
import Domination.Data.Play (Play(..), WirePlay)
import Domination.Data.Play as Play
import Domination.Data.Player as Player
import Domination.Data.Reaction (Reaction(..))
import Domination.Data.WireInt (WireInt(..), _WireInt)
import Domination.UI.RenderText (renderTextInContext)
import Halogen.HTML (ClassName(..), HTML)
import Halogen.HTML (text) as HH
import Halogen.HTML.Elements (div, span) as HH
import Halogen.HTML.Properties (class_) as HH

type Envelope = { id :: String, message :: Message }
type WireEnvelope = Tuple String WireMessage

wrapMessage :: String -> Message -> Envelope
wrapMessage id message = { id, message }

data Message
  = ChatMessage { username :: String, message :: String }
  | UsernameMessage { username :: String, id :: String }
  | GameStateMessage
    { i :: Int
    , state :: GameState
    , playMade :: Maybe
      { play :: Play
      , playerIndex :: Int
      , state :: GameState
      }
    }
  | PlayMadeMessage
    { play :: Play
    , playerIndex :: Int
    , state :: GameState
    }
  | SeenMessage String
  | ConnectionsMessage Int

derive instance genericMessage :: Generic Message _
instance showMessage :: Show Message where
  show = genericShow
instance encodeJsonMessage :: EncodeJson Message where
  encodeJson = genericEncodeJson
instance decodeJsonMessage :: DecodeJson Message where
  decodeJson = genericDecodeJson

data WireMessage
  = ChatWireMessage (Tuple String String)
  | UsernameWireMessage (Tuple String String)
  | GameStateWireMessage
    (Tuple WireInt
    (Tuple WireGameState
    (Maybe
    (Tuple WirePlay
    (Tuple WireInt WireGameState)))))
  | PlayMadeWireMessage
    (Tuple WirePlay
    (Tuple WireInt WireGameState))
  | SeenWireMessage String
  | ConnectionsWireMessage WireInt

mkConnectionsWireMessage :: Int -> WireMessage
mkConnectionsWireMessage = ConnectionsWireMessage <<< WireInt <<< Int8

_toWire :: Iso' Message WireMessage
_toWire = iso to from where
  to = case _ of
    ChatMessage { username, message } ->
      ChatWireMessage (Tuple username message)
    UsernameMessage { username, id} ->
      UsernameWireMessage (Tuple username id)
    GameStateMessage { i, state, playMade } ->
      GameStateWireMessage
      $ Tuple (view _WireInt i)
      $ Tuple (view GameState._toWire state) (pmm <$> playMade)
    PlayMadeMessage x ->
      PlayMadeWireMessage $ pmm x
    SeenMessage s ->
      SeenWireMessage s
    ConnectionsMessage i ->
      ConnectionsWireMessage $ view _WireInt i
    where
      pmm { play, playerIndex, state } =
        Tuple (view Play._toWire play)
        $ Tuple
          (view _WireInt playerIndex)
          (view GameState._toWire state)
  from = case _ of
    ChatWireMessage (Tuple username message) ->
      ChatMessage { username, message }
    UsernameWireMessage (Tuple username id) ->
      UsernameMessage { username, id}
    GameStateWireMessage (Tuple i (Tuple state maybePlayMade)) ->
      GameStateMessage
        { i: review _WireInt i
        , state: review GameState._toWire state
        , playMade: pmm <$> maybePlayMade
        }
    PlayMadeWireMessage x ->
      PlayMadeMessage $ pmm x
    SeenWireMessage s ->
      SeenMessage s
    ConnectionsWireMessage i ->
      ConnectionsMessage $ review _WireInt i
    where
      pmm (Tuple play (Tuple playerIndex state)) =
        { play: review Play._toWire play
        , playerIndex: review _WireInt playerIndex
        , state: review GameState._toWire state
        }

derive instance genericWireMessage :: Generic WireMessage _
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

renderHtml :: forall w i. Message -> HTML w i
renderHtml (ChatMessage { username, message }) =
  HH.div
    [ HH.class_ $ ClassName "chat-message" ]
    [ HH.span [ HH.class_ $ ClassName "username" ] [ HH.text username ]
    , HH.text ": "
    , HH.span [ HH.class_ $ ClassName "message" ] [ HH.text message ]
    ]
renderHtml (GameStateMessage _) =
  HH.div
    [ HH.class_ $ ClassName "game-state-message" ]
    [ HH.div [ HH.class_ $ ClassName "game-state" ] [ HH.text "(Game Data Received)" ]
    ]
renderHtml (SeenMessage address) =
  HH.div
    [ HH.class_ $ ClassName "seen-message" ]
    [ HH.text "(New Connection: "
    , HH.span [ HH.class_ $ ClassName "address" ] [ HH.text address ]
    , HH.text ")"
    ]
renderHtml (ConnectionsMessage count) =
  HH.div
    [ HH.class_ $ ClassName "connections-message" ]
    [ HH.text "("
    , HH.span [ HH.class_ $ ClassName "address" ] [ HH.text $ show count ]
    , HH.text " Connections)"
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
      [ HH.text $ "Player " <> show (player + 1) <> " "
      , HH.span [ HH.class_ $ ClassName "play-made" ] [ HH.text text ]
      ]
  where
      play' :: Maybe String
      play' = case play of
        NewGame { playerCount } -> Just $
          "created a new " <> show playerCount <> " player game"
        EndPhase { playerIndex } -> Nothing
        PlayCard { playerIndex, cardIndex } -> Just $
          "played: " <> getPlayerCardName playerIndex state cardIndex
        Purchase { playerIndex, stackIndex } -> Just $
          "purchased: " <> text
          where
            text = case preview (GameState._stack stackIndex) state of
              Nothing -> "???"
              Just { card } -> card.name
                <>
                case card.special of
                Nothing -> ""
                Just _ -> " ("
                  <> intercalate ", " (_.description <$> card.special)
                  <> ")"
        ResolveChoice { playerIndex, choice } ->
          Just $ renderTextInContext playerIndex state choice
        React { playerIndex, reaction } -> Just $
          case reaction of
            Nothing -> "did not react"
            Just BlockAttack -> "blocked an attack"

getPlayerCardName :: Int -> GameState -> Int -> String
getPlayerCardName playerIndex state cardIndex = fromMaybe "???"
  $ state ^? GameState._player playerIndex
    <<< Player._cardInHand cardIndex
    <<< Card._name

