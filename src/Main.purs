module Main where

import Prelude

import Control.Monad.State.Class (gets)
import Data.Array (take, (!!), (:))
import Data.Either (Either(..))
import Data.Foldable (length)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Domination.Data.GameState (GameState)
import Domination.UI.Css as Css
import Domination.UI.Domination (GameUpdate(..))
import Domination.UI.Domination as Domination
import Effect (Effect)
import Effect.Aff (makeAff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import FFI (Bugout)
import FFI as FFI
import Halogen (Component, ComponentSlot, Slot)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (ClassName(..), HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM (HalogenM)
import Halogen.VDom.Driver (runUI)
import Message (Message(..))
import Message as Message
import Storage as Storage
import Util (randomElement, readJson, writeJson)
import Web.Event.Event (Event, EventType(..), preventDefault)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, toEvent)
import Web.UIEvent.KeyboardEvent as KE

type AppState =
  { connectionCount :: Int
  , id :: String
  , username :: String
  , usernameMap :: HashMap String String
  , players :: Int
  , playerIndex :: Int
  , chatInputMessage :: String
  , messages :: Array Message
  , offer :: String
  , answer :: String
  , receivedAnswer :: Array String
  , message :: String
  , localDescription :: String
  , gameOn :: Boolean
  , gameState :: Maybe Domination.GameUpdate
  , text :: String
  , bugout :: Bugout
  , roomCode :: String
  }

newApp :: Bugout -> String -> String -> HashMap String String -> AppState
newApp bugout username uuid usernameMap =
  { connectionCount: 0
  , id: uuid
  , username: username
  , usernameMap
  , players: 1
  , playerIndex: 0
  , chatInputMessage: ""
  , messages: []
  , offer: ""
  , answer: ""
  , receivedAnswer: []
  , message: ""
  , localDescription: ""
  , gameOn: false
  , gameState: Nothing
  , text: ""
  , bugout: bugout
  , roomCode: globalRoomCode
  }

globalRoomCode :: String
globalRoomCode = "global"

uuidKey :: String
uuidKey = "player-id"

usernameKey :: String
usernameKey = "username"

emojis :: Array String
emojis = ["😄","😃","😀","😊","☺","😉","😍","😘","😚","😗","😙","😜","😝","😛","😳","😁","😔","😌","😒","😞","😣","😢","😂","😭","😪","😥","😰","😅","😓","😩","😫","😨","😱","😠","😡","😤","😖","😆","😋","😷","😎","😴","😵","😲","😟","😦","😧","😈","👿","😮","😬","😐","😕","😯","😶","😇","😏","😑","👲","👳","👮","👷","💂","👶","👦","👧","👨","👩","👴","👵","👱","👼","👸","😺","😸","😻","😽","😼","🙀","😿","😹","😾"]

main :: Effect Unit
main =  HA.runHalogenAff $ do
  eUuid <- liftEffect $ Storage.load uuidKey
  eUsername :: Either String String <- liftEffect $ Storage.load usernameKey
  uuid <- case eUuid of
    Left e -> do
      liftEffect $ Console.log("no existing uuid found, generating a new one")
      uuid <- liftEffect $ FFI.genUuid
      liftEffect $ Storage.save uuidKey uuid
      pure $ show uuid
    Right uuid -> pure uuid

  body <- HA.awaitBody

  username <- case eUsername of
    Left e -> do
      liftEffect $ Console.log("no existing username found, using default")
      emoji <- fromMaybe ":)" <$> randomElement emojis
      pure $ emoji <> "lurker" <> emoji
    Right u -> pure u

  let usernameMap = HashMap.insert uuid username HashMap.empty

  bugout :: Bugout <- makeAff $ FFI.makeBugout globalRoomCode Left Right
  let (initialState :: AppState) = newApp bugout username uuid usernameMap
  runUI (component initialState) (MakeNewGame 1) body

component :: forall m s query o. MonadAff m => AppState -> Component HTML query o s m
component state = H.mkComponent { eval, initialState, render } where
  eval = H.mkEval (H.defaultEval { handleAction = handleAction })
  initialState _ = state

type DominationComponent o c m =
  ComponentSlot HTML ("Domination" :: Slot o GameState Int | c) m AppAction

preventTyping :: forall x. HP.IProp (onKeyDown :: KeyboardEvent | x) AppAction
preventTyping = HE.onKeyDown \e -> Just $ PreventDefault (toEvent e)

incrementer :: forall w. Maybe Int -> Maybe Int -> Int -> (Int -> AppAction) -> HTML w AppAction
incrementer mbMin mbMax value setValue = HH.div
  [ HP.class_ $ Css.incrementer ]
  [ HH.button
    [ HE.onClick \_ -> case mbMin of
      Just min ->
        if value <= min
        then Just $ setValue min
        else Just $ setValue (value - 1)
      Nothing -> Just $ setValue (value - 1)
    ] [ HH.text "-" ]
  , HH.input $
    [ HP.value $ show value
    , HP.required true
    , preventTyping
    ]
    <> case mbMin of
      Just min -> [ HP.min $ toNumber min ]
      Nothing -> []
    <> case mbMin of
      Just max -> [ HP.max $ toNumber max ]
      Nothing -> []
  , HH.button
    [ HE.onClick $ \_ -> case mbMax of
      Just max ->
        if value >= max
        then Just $ setValue max
        else Just $ setValue (value + 1)
      Nothing -> Just $ setValue (value + 1)
    ]
    [ HH.text "+" ]
  ]

render :: forall o c m. MonadEffect m => AppState -> HTML (DominationComponent o c m) AppAction
render state = HH.main_ $
  [ HH.div [ HP.id_ "msg", HE.handler (EventType "msg") (Just <<< ReceiveMessage) ] []
  , HH.label_ [ HH.text "Username: " ]
  , HH.input
    [ HP.type_ HP.InputText
    , HP.value state.username
    , HP.placeholder usernameKey
    , HP.required true
    , HE.onValueInput $ Just <<< WriteUsername
    ]
  , HH.h1 [] [ HH.text $ show state.connectionCount <> " Users in Chat" ]
  , HH.div_
    [ HH.div
      [ HP.class_ $ ClassName "chat-history" ]
      $ Message.renderHtml <$>
        let um = state.usernameMap in
        let f = \x ->
              case x of
                ChatMessage { username, message } -> ChatMessage { message, username: fromMaybe username $ HashMap.lookup username um }
                y -> y in
        let messages = f <$> take 1000 state.messages
        in messages
    , HH.div
      [ HP.class_ $ ClassName "chat-form"]
      [ HH.button
        [ HE.onClick \_ -> Just SendMessage
        , HP.class_ $ ClassName "send-chat"
        ]
        [ HH.text "Send" ]
      , HH.span_
        [ HH.input
          [ HP.type_ HP.InputText
          , HP.class_ $ ClassName "chat-input"
          , HP.value state.chatInputMessage
          , HP.required true
          , HE.onValueInput $ Just <<< WriteMessage
          , HE.onKeyDown \e -> if (KE.key e) == "Enter" then Just SendMessage else Nothing
          ]
        ]
      ]
    ]
  , HH.div
    [ HP.class_ $ ClassName "container" ]
    [ HH.label_ [ HH.text "Players: " ]
    , incrementer (Just 1) Nothing state.players WritePlayerCount
    ]
  , HH.div
    [ HP.class_ $ ClassName "container" ]
    [ HH.label_ [ HH.text "Player #: " ]
    , incrementer (Just 1) Nothing (state.playerIndex + 1) ((_ - 1) >>> WritePlayerIndex)
    ]
  , HH.div_
    [ HH.button [ HE.onClick \_ -> Just $ StartNewGame ] [ HH.text $ "Start New " <> show state.players <> " Player Game as Player " <> show (state.playerIndex + 1) ]
    , HH.button [ HE.onClick \_ -> Just $ LoadGame ] [ HH.text "Load Game" ]
    ]
  ] <> case state.gameState of
    Nothing -> []
    Just (UpdateState gs) ->
      [ HH.slot (SProxy :: SProxy "Domination") 0 (Domination.component (length gs.players) state.playerIndex) (UpdateState gs) (Just <<< UpdateGameState) ]
    Just (MakeNewGame n) -> [ HH.slot (SProxy :: SProxy "Domination") 0 (Domination.component n state.playerIndex) (MakeNewGame n) (Just <<< UpdateGameState) ]

data AppAction = MakeOffer Int
  | CopyToClipboard String
  | WriteUsername String
  | WritePlayerIndex Int
  | WritePlayerCount Int
  | WriteOffer String
  | AcceptOffer Int
  | WriteAnswer Int String
  | AcceptAnswer Int
  | SendMessage
  | WriteMessage String
  | ReceiveMessage Event
  | LoadGame
  | StartNewGame
  | UpdateGameState GameState
  | PreventDefault Event

handleAction :: forall slots output m. MonadAff m => AppAction -> HalogenM AppState AppAction slots output m Unit
handleAction = case _ of
  PreventDefault e -> liftEffect $ preventDefault e
  CopyToClipboard id -> liftEffect $ FFI.copyToClipboard id
  MakeOffer i -> do
    ld <- liftAff $ makeAff $ FFI.create i Right
    H.modify_ _ { localDescription = ld }
  WriteOffer rd -> do
    H.modify_ \state -> state { offer = rd }
  AcceptOffer i -> do
    s <- H.get
    ld <- liftAff $ makeAff $ FFI.join s.offer Right
    H.modify_ \state -> state { answer = ld, playerIndex = i + 1 }
  WriteAnswer _ rd -> do
    H.modify_ \state -> state { receivedAnswer = state.receivedAnswer <> [ rd ]}
  WriteUsername username -> do
    s <- H.get
    let (m :: Message) = UsernameMessage { username, id: s.id }

    Storage.save "username" username
    sendMessage m
    let um = HashMap.insert s.id username s.usernameMap
    H.modify_ \state -> state { username = username, usernameMap = um }
  WritePlayerIndex playerIndex -> do
    state <- H.get
    if playerIndex > 0
    then H.modify_ _{ playerIndex = playerIndex }
    else H.modify_ \s -> s { playerIndex = 0 }
    if playerIndex >= state.players
    then H.modify_ _{ players = playerIndex + 1 }
    else pure unit
  WritePlayerCount players -> do
      state <- H.get
      if players > 1
      then H.modify_ _{ players = players }
      else H.modify_ \s -> s { players = 1 }
      if players <= state.playerIndex
      then H.modify_ _{ playerIndex = players - 1 }
      else pure unit
  AcceptAnswer i -> do
    s <- H.get
    let ra = s.receivedAnswer !! i
    case ra of
      Nothing -> liftEffect $ Console.error $ "Cannot accept answer (" <> show i <> ") of (" <> show ((length s.receivedAnswer) :: Int) <> ")"

      Just answer -> do
        liftEffect $ FFI.gotAnswer i answer
        H.modify_ \state -> state { chatInputMessage = "PING", players = state.players + 1 }
  WriteMessage s -> do
    H.modify_ \state -> state { chatInputMessage = s }
  SendMessage -> sendChatMessage
  ReceiveMessage customEvent -> do
    let (ePackage :: Either String Message.Envelope) = readJson $ FFI.detail customEvent
    case ePackage of
      Left e -> liftEffect $ Console.log $ "problem receiving message: " <> e
      Right { id, message: mt } -> do
        case mt of
            UsernameMessage { username, id: i } -> do
              s <- H.get
              let usernameMap = HashMap.insert i username s.usernameMap
              H.put s { usernameMap = usernameMap }

              liftEffect $ Console.log $ "username incoming "
              liftEffect $ Console.log $ "username map: " <> show usernameMap
            SeenMessage address ->
              liftEffect $ Console.log $ "I see you: " <> address
            ConnectionsMessage count ->
              H.modify_ \state -> state { connectionCount = count }
            GameStateMessage gs -> do
              H.modify_ \state -> state { gameState = Just $ UpdateState gs, messages = mt : state.messages }
            ChatMessage { message, username } -> do
              H.modify_ \state -> state { messages = mt : state.messages }
              if message == "PING"
                then do
                  H.modify_ _ { chatInputMessage = "PONG" }
                  sendChatMessage
                else pure unit
  LoadGame -> do
    mbGameState <- Storage.load "game_state"
    case mbGameState of
      Left e -> liftEffect $ Console.log e
      Right gameState -> do
        H.modify_ _ { gameState = Just $ UpdateState gameState }
        sendMessage $ GameStateMessage gameState
  StartNewGame -> do
    state <- H.get
    liftEffect $ Console.log $ "MakeNewGame " <> show state.players
    H.modify_ _ { gameState = Just $ MakeNewGame state.players }
  UpdateGameState gameState -> do
    s <- H.modify_ _ { gameState = Just $ UpdateState gameState }
    sendMessage $ GameStateMessage gameState
    Storage.save "game_state" gameState
  where
    sendChatMessage = do
      s <- H.get
      let message = ChatMessage { username: s.id, message: s.chatInputMessage }
      sendMessage message
      H.modify_ \state -> state { messages = message : state.messages, chatInputMessage = "" }

    sendMessage message = do
      bugout <- gets _.bugout
      id :: String <- H.gets _.id
      let package = { id, message }
      liftEffect $ FFI.send bugout $ writeJson package

