module Main where

import Prelude

import Halogen.Query.HalogenM (HalogenM)
import Data.Either (Either(..))
import Data.Array (length, mapWithIndex, take, updateAt, (!!), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (makeAff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen (Component)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Halogen.Query.EventSource as ES
import Web.Event.Event (EventType(..), Event)
import Web.UIEvent.KeyboardEvent as KE

import Dominion (Card, GameState, Phase(..), Player, Stack, cleanup, newGame, next, play, purchase, score, value)
import Comm as Comm

type AppState =
  { chatInputMessage :: String
  , messages :: Array String
  , offer :: String
  , answer :: String
  , receivedAnswer :: String
  , message :: String
  , localDescription :: String
  , gameState :: GameState
  , text :: String
  }
newApp :: AppState
newApp =
  { chatInputMessage: ""
  , messages: []
  , offer: ""
  , answer: ""
  , receivedAnswer: ""
  , message: ""
  , localDescription: "nada"
  , gameState: newGame
  , text: ""
  }

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

component :: forall m a b c. MonadAff m => Component HTML a b c m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState :: b -> AppState
  initialState _ = newApp

render :: forall b. AppState -> HTML b Action
render state =
  HH.div_
    [ HH.div [ HP.id_ "msg", HE.handler (EventType "msg") (\e -> Just $ MessageEventReceived e) ] []
    , HH.h1 [] [ HH.text "Creator" ]
    , HH.button [ HE.onClick \_ -> Just Create ] [ HH.text "Create" ]
    , HH.text state.localDescription
    , HH.input
      [ HP.type_ HP.InputText
      , HP.value "put joiner's answer here"
      , HP.required true
      , HE.onValueInput $ Just <<< SetAnswer
      ]
    , HH.button [ HE.onClick \_ -> Just GotAnswer ] [ HH.text "Got Answer" ]
    , HH.h1 [] [ HH.text "Joiner" ]
    , HH.input
      [ HP.type_ HP.InputText
      , HP.value "put creater's offer here"
      , HP.required true
      , HE.onValueInput $ Just <<< SetOffer
      ]
    , HH.button [ HE.onClick \_ -> Just Join ] [ HH.text "Join" ]
    , HH.text state.answer
    , HH.h1 [] [ HH.text "Chat" ]
    , HH.input
      [ HP.type_ HP.InputText
      , HP.value state.chatInputMessage
      , HP.required true
      , HE.onValueInput $ Just <<< SetMessage
      , HE.onKeyDown \e -> if (KE.key e) == "Enter" then Just SendMessage else Nothing
      ]
    , HH.button [ HE.onClick \_ -> Just SendMessage ] [ HH.text "Send" ]
    , HH.div_ $ map (\m -> HH.p [] [ HH.text m ]) (take 5 state.messages)
    , HH.h1 [] [ HH.text "Domination" ]
    , HH.button [ HE.onClick \_ -> Just NewGame ] [ HH.text "New Game" ]
    , HH.div_ $ renderPlayers state.gameState
    , HH.div_ [ HH.h2 [] [ HH.text "Game State" ] ]
    , HH.div_ [ HH.text $ show state ]
    ]

renderSupply :: forall a. Int -> Player -> GameState -> Array (HTML a Action)
renderSupply playerIndex player state =
  map (renderCardInSupply playerIndex player) state.supply

renderCardInSupply :: forall a. Int -> Player -> Stack -> HTML a Action
renderCardInSupply playerIndex player stack = HH.button [ HE.onClick \_ -> Just $ Purchase playerIndex player stack ] [ HH.text stack.card.name ]

renderPlayers :: forall a. GameState -> Array (HTML a Action)
renderPlayers state =
  [ HH.p [] [ HH.text $ "Turn: Player " <> show state.turn ]
  , HH.p [] [ HH.text $ "Phase: " <> show state.phase ]
  ]
  <> (renderPlayer state) `mapWithIndex` state.players

renderPlayer :: forall a. GameState -> Int -> Player -> HTML a Action
renderPlayer state playerIndex player = HH.div_
  [ HH.h2 [] [ HH.text $ "Player " <> (show playerIndex) <> " (VP: " <> show (score player) <> ")" ]
  , HH.button [ HE.onClick \_ -> Just $ NextPhase playerIndex ] [ HH.text "Next Phase" ]
  , HH.h3 [] [ HH.text $ "Supply ($" <> show (value player.atPlay - value player.buying) <> ")"  ]
  , HH.div_ $ renderSupply playerIndex player state
  , HH.h3 [] [ HH.text $ "Deck" ]
  , HH.div_ $ map renderCardView player.deck
  , HH.h3 [] [ HH.text $ "Cards in Hand" ]
  , HH.div_ $ mapWithIndex renderCardInHand (map (\x -> Tuple playerIndex x) player.hand)
  , HH.h3 [] [ HH.text $ "Cards at Play" ]
  , HH.div_ $ map renderCardView player.atPlay
  , HH.h3 [] [ HH.text $ "Discard" ]
  , HH.div_ $ map renderCardView player.discard
  ]

renderCardView :: forall a. Card -> HTML a Action
renderCardView card = HH.label_ [HH.text card.name]

renderCardInHand :: forall a. Int -> Tuple Int Card -> HTML a Action
renderCardInHand cardIndex (Tuple playerIndex card) = HH.button [HE.onClick \_ -> Just (Play playerIndex cardIndex)] [ HH.text card.name ]

data Action =
  Create
  | Join
  | GotAnswer
  | SendMessage
  | SetMessage String
  | MessageEventReceived Event
  | SetOffer String
  | SetAnswer String
  | NewGame
  | NextPhase Int
  | Play Int Int
  | Purchase Int Player Stack

type Setup = ES.Emitter Effect Action -> Effect (ES.Finalizer Effect)
handleAction :: forall o m. MonadAff m
  => Action
  -> HalogenM AppState Action () o m Unit
handleAction = case _ of
  Create -> do
    ld <- liftAff $ makeAff $ (Comm.create Right)
    H.modify_ \state -> state { localDescription = ld }
  MessageEventReceived customEvent -> do
    let message = Comm.detail $ customEvent
    let remoteMessage = "<- " <> message
    H.modify_ \state -> state { messages = remoteMessage : state.messages }
  Join -> do
    s <- H.get
    ld <- liftAff $ makeAff $ Comm.join s.offer Right
    H.modify_ \state -> state { answer = ld }
  SetAnswer rd -> do
    liftEffect $ Comm.log rd
    H.modify_ \state -> state { receivedAnswer = rd }
  GotAnswer -> do
    s <- H.get
    liftEffect $ Comm.gotAnswer s.receivedAnswer
  SetMessage s -> do
    H.modify_ \state -> state { chatInputMessage = s }
  SendMessage -> do
    s <- H.get
    liftEffect $ Comm.say s.chatInputMessage
    let localMessage = "-> " <> s.chatInputMessage
    H.modify_ \state -> state { messages = localMessage : state.messages, chatInputMessage = "" }
  SetOffer rd -> do
    liftEffect $ Comm.log rd
    H.modify_ \state -> state { offer = rd }
  NewGame -> H.modify_ \state -> newApp
  NextPhase playerIndex -> H.modify_ \(state :: AppState) ->
    if playerIndex == state.gameState.turn
    then state
      { gameState = state.gameState
        { phase = next state.gameState.phase
        , turn =
          if state.gameState.phase == Cleanup
          then (state.gameState.turn + 1) `mod` (length state.gameState.players)
          else state.gameState.turn
        , players =
            if state.gameState.phase == Cleanup
            then mapWithIndex (\i p -> if i == playerIndex then cleanup p else p) state.gameState.players
            else state.gameState.players
        }
      }
    else state { text = "Error: not your turn!" }
  Play player card -> H.modify_ \state -> case f state of
    Nothing -> state { text = "error" }
    Just state -> state
    where
      f state =
        if player == state.gameState.turn
        then do
          player' <- state.gameState.players !! player
          player'' <- play player' card
          players' <- updateAt player player'' state.gameState.players
          pure $ state { gameState = state.gameState { players = players' }, text = "good" }
        else pure state { text = "Error: not your turn!" }
  Purchase playerIndex player stack -> H.modify_ \state ->
    case purchase playerIndex player stack state.gameState of
      Nothing -> state { text = "Error trying to buy card!" }
      Just gameState -> state { gameState = gameState, text = "good" }

