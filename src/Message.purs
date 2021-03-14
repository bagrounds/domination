module Message where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, genericByteLength, genericPutArrayBuffer, genericReadArrayBuffer)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens.Fold (preview, (^?))
import Data.Lens.Getter (view)
import Data.Lens.Iso (Iso', iso)
import Data.Lens.Prism (review)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Domination.Data.Card as Card
import Domination.Data.GameState (GameState)
import Domination.Data.GameState (_player, _stack) as GameState
import Domination.Data.Play (Play(..), WirePlay)
import Domination.Data.Play as Play
import Domination.Data.Player as Player
import Domination.Data.Reaction (Reaction(..))
import Domination.Data.Wire.GameState (WireGameState)
import Domination.Data.Wire.GameState (_toWire) as GameState
import Domination.Data.WireInt (WireInt, _WireInt)
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

_toWire :: Iso' RemoteMessage WireMessage
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
    where
      pmm (Tuple play (Tuple playerIndex state)) =
        { play: review Play._toWire play
        , playerIndex: review _WireInt playerIndex
        , state: review GameState._toWire state
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
    [ HH.span [ HH.class_ $ ClassName "username" ] [ HH.text username ]
    , HH.text ": "
    , HH.span [ HH.class_ $ ClassName "message" ] [ HH.text message ]
    ]
renderHtml (GameStateMessage _) =
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
      play' :: forall w i. Maybe (HTML w i)
      play' = case play of
        NewGame { playerCount } -> Just $
          HH.span_
            [ HH.text $ "created a new "
              <> show playerCount <> " player game"
            ]
        EndPhase { playerIndex } -> Nothing
        PlayCard { playerIndex, cardIndex } -> Just $
          HH.text $ "played: "
            <> getPlayerCardName playerIndex state cardIndex
        Purchase { playerIndex, stackIndex } -> Just $
          HH.text $ "purchased: " <> text
          where
            text = case preview (GameState._stack stackIndex) state of
              Nothing -> "???"
              Just { card } -> card.name
        ResolveChoice { playerIndex, choice } ->
          Just $ renderTextInContext playerIndex state choice
        React { playerIndex, reaction } -> Just $
          HH.text $ case reaction of
            Nothing -> "did not react"
            Just BlockAttack -> "blocked an attack"

getPlayerCardName :: Int -> GameState -> Int -> String
getPlayerCardName playerIndex state cardIndex = fromMaybe "???"
  $ state ^? GameState._player playerIndex
    <<< Player._cardInHand cardIndex
    <<< Card._name

