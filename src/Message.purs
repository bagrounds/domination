module Message where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Array (intercalate, null)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens.Fold (preview, (^?))
import Data.Maybe (Maybe(..), fromMaybe)
import Domination.Data.Card as Card
import Domination.Data.GameState (GameState)
import Domination.Data.GameState as GameState
import Domination.Data.Play (Play(..))
import Domination.Data.Player as Player
import Domination.Data.Reaction (Reaction(..))
import Domination.UI.RenderText (renderTextInContext)
import Halogen.HTML (ClassName(..), HTML)
import Halogen.HTML (text) as HH
import Halogen.HTML.Elements (div, span) as HH
import Halogen.HTML.Properties (class_) as HH

type Envelope = { id :: String, message :: Message }

wrapMessage :: String -> Message -> Envelope
wrapMessage id message = { id, message }

data Message
  = ChatMessage { username :: String, message :: String }
  | UsernameMessage { username :: String, id :: String }
  | GameStateMessage { state :: GameState, i :: Int }
  | PlayMadeMessage { play :: Play, playerIndex :: Int, state :: GameState }
  | SeenMessage String
  | ConnectionsMessage Int

derive instance genericMessage :: Generic Message _
instance showMessage :: Show Message where
  show = genericShow
instance encodeJsonMessage :: EncodeJson Message where
  encodeJson = genericEncodeJson
instance decodeJsonMessage :: DecodeJson Message where
  decodeJson = genericDecodeJson

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
                if null card.specials
                then ""
                else " ("
                <> intercalate ", " (_.description <$> card.specials)
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

