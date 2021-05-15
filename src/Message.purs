module Message where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, genericByteLength, genericPutArrayBuffer, genericReadArrayBuffer)
import Data.Generic.Rep (class Generic)
import Data.Lens.Fold (preview, (^?))
import Data.Lens.Getter (view, (^.))
import Data.Lens.Iso (Iso', iso)
import Data.Lens.Prism (review)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import Domination.Data.Card as Card
import Domination.Data.Game (Game)
import Domination.Data.Game (_player, _stack) as Game
import Domination.Data.Play (Play(..))
import Domination.Data.Player as Player
import Domination.Data.Reaction (Reaction(..))
import Domination.Data.Wire.Game (WireGame)
import Domination.Data.Wire.Game (_toWire) as Game
import Domination.Data.Wire.Int (WireInt)
import Domination.Data.Wire.Int as Int
import Domination.Data.Wire.Play (WirePlay)
import Domination.Data.Wire.Play (_toWire) as Play
import Domination.UI.RenderText (renderTextInContext)
import Halogen.HTML (ClassName(..), HTML)
import Halogen.HTML (text) as HH
import Halogen.HTML.Elements (div, span, span_) as HH
import Halogen.HTML.Properties (class_) as HH
import Util ((.^))

type Envelope = { id :: String, message :: RemoteMessage }
type WireEnvelope = Tuple String WireMessage

wrapMessage :: String -> RemoteMessage -> Envelope
wrapMessage id message = { id, message }

data RemoteMessage
  = ChatMessage
    { message :: String
    , username :: String
    }
  | UsernameMessage
    { id :: String
    , username :: String
    }
  | GameMessage
    { i :: Int
    , playerIndex :: Int
    , playMade :: Maybe Play
    , state :: Game
    }
  | PlayMadeMessage
    { play :: Play
    , playerIndex :: Int
    , state :: Game
    }
  | NewGameMessage
    { playerCount :: Int
    , playerIndex :: Int
    }

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
  = ChatWireMessage String String
  | UsernameWireMessage String String
  | GameWireMessage WireInt WireInt (Maybe WirePlay) WireGame
  | PlayMadeWireMessage WirePlay WireInt WireGame
  | NewGameWireMessage WireInt WireInt

_toWire :: Iso' RemoteMessage WireMessage
_toWire = iso to from where
  to = case _ of
    ChatMessage { message, username } ->
      ChatWireMessage message username

    UsernameMessage { id, username } ->
      UsernameWireMessage id username

    GameMessage { i, playerIndex, playMade, state } ->
      GameWireMessage
      (view Int._toWire i)
      (view Int._toWire playerIndex)
      (view Play._toWire <$> playMade)
      (view Game._toWire state)

    PlayMadeMessage { play, playerIndex, state } ->
      PlayMadeWireMessage
        (play ^. Play._toWire)
        (playerIndex ^. Int._toWire)
        (state ^. Game._toWire)

    NewGameMessage { playerCount, playerIndex } ->
      NewGameWireMessage
        (playerCount ^. Int._toWire)
        (playerIndex ^. Int._toWire)

  from = case _ of
    ChatWireMessage message username ->
      ChatMessage { message, username }

    UsernameWireMessage id username ->
      UsernameMessage { id, username }

    GameWireMessage i playerIndex playMade state ->
      GameMessage
        { i: review Int._toWire i
        , playerIndex: playerIndex .^ Int._toWire
        , playMade: review Play._toWire <$> playMade
        , state: review Game._toWire state
        }

    PlayMadeWireMessage play playerIndex state ->
      PlayMadeMessage
        { play: play .^ Play._toWire
        , playerIndex: playerIndex .^ Int._toWire
        , state: state .^ Game._toWire
        }

    NewGameWireMessage playerCount playerIndex ->
      NewGameMessage
        { playerCount: playerCount .^ Int._toWire
        , playerIndex: playerIndex .^ Int._toWire
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
renderHtml (ChatMessage { username, message }) =
  HH.div
    [ HH.class_ $ ClassName "chat-message" ]
    [ HH.span
      [ HH.class_ $ ClassName "username" ]
      [ HH.text username ]
    , HH.text ": "
    , HH.span [ HH.class_ $ ClassName "message" ] [ HH.text message ]
    ]
renderHtml (GameMessage _) =
  HH.div
    [ HH.class_ $ ClassName "game-state-message" ]
    [ HH.div
      [ HH.class_ $ ClassName "game-state" ]
      [ HH.text "(Game Data Received)" ]
    ]
renderHtml (UsernameMessage { username, id }) =
  HH.div
    [ HH.class_ $ ClassName "username-message" ]
    [ HH.text "("
    , HH.span
      [ HH.class_ $ ClassName "username" ]
      [ HH.text $ show username ]
    , HH.text " has ID: "
    , HH.span
      [ HH.class_ $ ClassName "username" ]
      [ HH.text $ show id ]
    , HH.text ")"
    ]

renderHtml (NewGameMessage { playerIndex, playerCount }) =
  HH.div
    [ HH.class_ $ ClassName "play-made-message" ]
    [ HH.text $ "Player " <> show (playerIndex + 1) <> ": "
    , HH.span
      [ HH.class_ $ ClassName "play-made" ]
      [ HH.span_
        [ HH.text $ "created a new "
          <> show playerCount <> " player game"
        ]
      ]
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
        React { playerIndex, reaction: maybeReaction } -> Just $
          case maybeReaction of
            Nothing -> HH.text $ "did not react"
            Just reaction -> case reaction of
              BlockAttack -> HH.text $ "blocked an attack"
              ReactWithChoice choice ->
                HH.span_ [ HH.text $ "reacted with: "
                , renderTextInContext playerIndex state choice
                ]
        DoneReacting { playerIndex } -> Just $
          HH.text $ "player " <> show (playerIndex + 1)
            <> " is done reacting"

getPlayerCardName :: Int -> Game -> Int -> String
getPlayerCardName playerIndex state cardIndex = fromMaybe "???"
  $ state ^? Game._player playerIndex
    <<< Player._cardInHand cardIndex
    <<< Card._name

