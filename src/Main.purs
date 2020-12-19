module Main where

import Prelude

import Halogen.Query.HalogenM (HalogenM, subscribe)
import Control.MonadZero
import Control.Monad.Trans.Class
import Data.Either
import Control.Monad.State.Class (class MonadState)
import Data.Array (take, drop, filter, length, deleteAt, mapWithIndex, replicate, updateAt, (!!), (:))
import Data.Foldable (foldr)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff (makeAff)
import Effect.Aff.Class (class MonadAff)
import Effect.Aff.Class
import Halogen (Component)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Halogen.Query.EventSource (EventSource)
import Halogen.Query.EventSource as ES
import Web.DOM.Element
import Web.DOM.Node
import Web.DOM.ParentNode
import Web.DOM.Text as WDT
import Web.Event.Event (EventType(..), Event)
import Web.HTML.HTMLElement (toElement)

import Comm as Comm

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

type GameState =
  { messages :: Array String
  , dataChannel :: Maybe Comm.DataChannel
  , peerConnection :: Maybe Comm.PeerConnection
  , offer :: String
  , answer :: String
  , receivedAnswer :: String
  , message :: String
  , localDescription :: String
  , turn :: Int
  , phase :: Phase
  , text :: String
  , players :: Array Player
  , supply :: Supply
  }

type Supply = Array Stack
type Stack = { card :: Card, count :: Int }

data Phase = Action | Buy | Cleanup

derive instance eqPhase :: Eq Phase

next :: Phase -> Phase
next Action = Buy
next Buy = Cleanup
next Cleanup = Action

derive instance genericPhase :: Generic Phase _
instance showPhase :: Show Phase where show = genericShow

newGame :: GameState
newGame =
  { messages: []
  , dataChannel: Nothing
  , peerConnection: Nothing
  , offer: ""
  , answer: ""
  , receivedAnswer: ""
  , message: ""
  , localDescription: "nada"
  , turn: 0
  , phase: Action
  , text: ""
  , players: [newPlayer, newPlayer]
  , supply:
    [ { card: copper, count: 50 }
    , { card: silver, count: 50 }
    , { card: gold, count: 50 }
    , { card: estate, count: 8 }
    , { card: duchy, count: 8 }
    , { card: province, count: 8 }
    ]
  }

type Player =
  { deck :: Array Card
  , hand :: Array Card
  , discard :: Array Card
  , toDiscard :: Array Card
  , atPlay :: Array Card
  , buying :: Array Card
  }

value :: Array Card -> Int
value = foldr (+) 0 <<< map _.treasure

purchase :: Stack -> Player -> Maybe (Tuple Player Stack)
purchase stack player =
  if stack.count == 0
  then Nothing
  else
    if (foldr (+) 1 <<< map _.buys) player.atPlay <= length player.buying
    then Nothing
    else
      if (value player.atPlay) - (value player.buying) < stack.card.cost
      then Nothing
      else Just (Tuple
        (player { buying = stack.card : player.buying })
        (stack { count = stack.count - 1 }))

play :: Player -> Int -> Maybe Player
play x i = do
  card <- x.hand !! i
  hand' <- deleteAt i x.hand
  let atPlay' = card : x.atPlay
  pure x { hand = hand', atPlay = atPlay' }

newPlayer :: Player
newPlayer =
  { deck: (replicate 2 copper) <> (replicate 3 estate)
  , hand: replicate 5 copper
  , discard: []
  , toDiscard: []
  , atPlay: []
  , buying: []
  }

type Card =
  { name :: String
  , cost :: Int
  , victoryPoints :: Int
  , treasure :: Int
  , buys :: Int
  , cards :: Int
  , actions :: Int
  }

estate :: Card
estate = { name: "Estate", cost: 2, victoryPoints: 1, treasure: 0, buys: 0, cards: 0, actions: 0 }
duchy :: Card
duchy = { name: "Duchy", cost: 5, victoryPoints: 3, treasure: 0, buys: 0, cards: 0, actions: 0 }
province :: Card
province = { name: "Province", cost: 8, victoryPoints: 6, treasure: 0, buys: 0, cards: 0, actions: 0 }
copper :: Card
copper = { name: "Copper", cost: 0, victoryPoints: 0, treasure: 1, buys: 0, cards: 0, actions: 0 }
silver :: Card
silver = { name: "Silver", cost: 3, victoryPoints: 0, treasure: 2, buys: 0, cards: 0, actions: 0 }
gold :: Card
gold = { name: "Gold", cost: 6, victoryPoints: 0, treasure: 3, buys: 0, cards: 0, actions: 0 }

component :: forall m a b c. MonadAff m => Component HTML a b c m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  --initialState :: b -> GameState
  initialState _ = newGame

id =
  { remoteOffer: "remote-offer"
  }

render :: forall b. GameState -> HTML b Action
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
      , HP.value "message"
      , HP.required true
      , HE.onValueInput $ Just <<< SetMessage
      ]
    , HH.button [ HE.onClick \_ -> Just SendMessage ] [ HH.text "Send" ]
    , HH.h1 [] [ HH.text "Domination" ]
    , HH.button [ HE.onClick \_ -> Just NewGame ] [ HH.text "New Game" ]
    , HH.div_ $ renderPlayers state
    , HH.div_ $
      (HH.h2 [] [ HH.text "Messages" ]) : (map (\m -> HH.p [] [ HH.text m ]) state.messages)
    , HH.div_ [ HH.h2 [] [ HH.text "Game State" ] ]
    , HH.div_ [ HH.text $ show state ]
    ]

renderSupply :: forall a. Int -> Player -> GameState -> Array (HTML a Action)
renderSupply playerIndex player state =
  map (renderCardInSupply playerIndex player) state.supply

renderCardInSupply :: forall a. Int -> Player -> Stack -> HTML a Action
renderCardInSupply playerIndex player stack = HH.button [ HE.onClick \_ -> Just $ Purchase playerIndex player stack ] [ HH.text stack.card.name ]

allCards :: Player -> Array Card
allCards player = player.hand <> player.deck <> player.atPlay <> player.discard <> player.toDiscard <> player.buying

score :: Player -> Int
score player = foldr (+) 0 $ map _.victoryPoints (allCards player)

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
  | MessageReceived String
  | MessageEventReceived Event

  | SetOffer String
  | SetAnswer String
  | SetRemoteDescription
  | NewGame
  | NextPhase Int
  | Play Int Int
  | Purchase Int Player Stack

cleanup :: Player -> Player
cleanup player = let
  discard' = player.discard <> player.atPlay <> player.hand <> player.toDiscard <> player.buying
  shouldShuffle = length player.deck < 5
  discard'' = if shouldShuffle then [] else discard'
  deck' = if shouldShuffle then player.deck <> discard' else player.deck
  hand' = take 5 deck'
  deck'' = drop 5 deck' in
  player { deck = deck'', hand = hand', discard = discard'', atPlay = [], buying = [], toDiscard = [] }

--events :: forall m. MonadAff m => EventSource m Action

type Setup = ES.Emitter Effect Action -> Effect (ES.Finalizer Effect)
handleAction :: forall o m. MonadAff m
  => Action
  -> HalogenM GameState Action () o m Unit
handleAction = case _ of
  Create -> do
    ld <- liftAff $ makeAff $ (Comm.create Right)
    H.modify_ \state -> state { localDescription = ld }
  MessageReceived message -> do
    liftEffect $ Comm.log message
  MessageEventReceived customEvent -> do
    let eventDetail = Comm.detail $ customEvent
    liftEffect $ Comm.log eventDetail
    H.modify_ \state -> state { messages = eventDetail : state.messages }
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
    H.modify_ \state -> state { message = s }
  SendMessage -> do
    s <- H.get
    liftEffect $ Comm.say s.message
  SetOffer rd -> do
    liftEffect $ Comm.log rd
    H.modify_ \state -> state { offer = rd }


  SetRemoteDescription -> do
    s <- H.get
    let rd = s.answer
    let pc = s.peerConnection
    liftEffect $ Comm.log rd
    case pc of
      Nothing -> do
        liftEffect $ Comm.log "no peer connection!"
        pure unit
      Just pc -> do
        liftEffect $ Comm.setRemoteDescription rd pc
        pure unit
  NewGame -> H.modify_ \state -> newGame
  NextPhase playerIndex -> H.modify_ \state ->
    if playerIndex == state.turn
    then state
      { phase = next state.phase
      , turn =
        if state.phase == Cleanup
        then (state.turn + 1) `mod` (length state.players)
        else state.turn
      , players =
          if state.phase == Cleanup
          then mapWithIndex (\i p -> if i == playerIndex then cleanup p else p) state.players
          else state.players
      }
    else state { text = "Error: not your turn!" }
  Play player card -> H.modify_ \state -> case f state of
    Nothing -> state { text = "error" }
    Just gameState -> gameState
    where
      f state =
        if player == state.turn
        then do
          player' <- state.players !! player
          player'' <- play player' card
          players' <- updateAt player player'' state.players
          pure $ state { players = players', text = "good" }
        else pure state { text = "Error: not your turn!" }
  Purchase playerIndex player stack -> H.modify_ \state ->
    if playerIndex == state.turn
    then
      if state.phase == Buy
      then case (do
        (Tuple player' stack') <- purchase stack player
        players' <- updateAt playerIndex player' state.players
        let supply' = stack' : (filter ((/=) stack) state.supply)
        pure state { players = players', supply = supply', text = "good" })
          of
          Nothing -> state { text = "Error trying to buy card!" }
          Just state -> state
      else state { text = "Error: wrong phase!" }
    else state { text = "Error: not your turn!" }

