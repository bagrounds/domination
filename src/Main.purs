module Main where

import Prelude

import Control.Monad.State.Class (class MonadState)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (take, (!!), (:))
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (length)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Domination.Data.GameState (GameState)
import Domination.UI.Domination (GameUpdate(..))
import Domination.UI.Domination as Domination
import Effect (Effect)
import Effect.Aff (makeAff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import FFI as FFI
import Halogen (Component, ComponentSlot, Slot)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Storage as Storage
import Web.Event.Event (EventType(..), Event)
import Web.UIEvent.KeyboardEvent as KE

type AppState =
  { username :: String
  , players :: Int
  , playerIndex :: Int
  , chatInputMessage :: String
  , messages :: Array String
  , offer :: String
  , answer :: String
  , receivedAnswer :: Array String
  , message :: String
  , localDescription :: String
  , gameOn :: Boolean
  , gameState :: Maybe Domination.GameUpdate
  , text :: String
  }

newApp :: AppState
newApp =
  { username: ""
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
  }

main :: Effect Unit
main = HA.runHalogenAff (runUI component unit =<< HA.awaitBody)

component :: forall m a b c. MonadAff m => Component HTML a b c m
component = H.mkComponent { eval, initialState, render } where
  eval = H.mkEval (H.defaultEval { handleAction = handleAction })
  initialState = const newApp

type DominationComponent o c m =
  ComponentSlot HTML ("Domination" :: Slot o GameState Int | c) m AppAction

render :: forall o c m. MonadEffect m => AppState -> HTML (DominationComponent o c m) AppAction
render state = HH.main_ $
  [ HH.input
    [ HP.type_ HP.InputText
    , HP.value state.username
    , HP.placeholder "username (unused)"
    , HP.required true
    , HE.onValueInput $ Just <<< WriteUsername
    ]
  , HH.div [ HP.id_ "msg", HE.handler (EventType "msg") (Just <<< ReceiveMessage) ] []

  , HH.h1 [] [ HH.text "Creator" ]
  , HH.button [ HE.onClick \_ -> Just $ MakeOffer 0 ] [ HH.text "Make Offer" ]
  , HH.textarea [ HP.placeholder "offer will appear here", HP.id_ "offer-text", HP.value state.localDescription ]
  , HH.button [ HE.onClick \_ -> Just $ CopyToClipboard "offer-text" ] [ HH.text "Copy Offer" ]
  , HH.input
    [ HP.type_ HP.InputText
    , HP.placeholder "put joiner's answer here"
    , HP.required true
    , HE.onValueInput $ Just <<< (WriteAnswer 0)
    , HE.onKeyDown \e -> if (KE.key e) == "Enter" then Just $ AcceptAnswer 0 else Nothing
    ]
  , HH.button [ HE.onClick \_ -> Just $ AcceptAnswer 0 ] [ HH.text "Accept Answer 1" ]

  , HH.button [ HE.onClick \_ -> Just $ MakeOffer 1 ] [ HH.text "Make Offer 2" ]
  , HH.textarea [ HP.placeholder "offer will 2 appear here", HP.id_ "offer-text-2", HP.value state.localDescription ]
  , HH.button [ HE.onClick \_ -> Just $ CopyToClipboard "offer-text-2" ] [ HH.text "Copy Offer 2" ]
  , HH.input
    [ HP.type_ HP.InputText
    , HP.placeholder "put joiner 2's answer here"
    , HP.required true
    , HE.onValueInput $ Just <<< (WriteAnswer 1)
    , HE.onKeyDown \e -> if (KE.key e) == "Enter" then Just $ AcceptAnswer 1 else Nothing
    ]
  , HH.button [ HE.onClick \_ -> Just $ AcceptAnswer 1 ] [ HH.text "Accept Answer 2" ]

  , HH.h1 [] [ HH.text "Joiner" ]
  , HH.input
    [ HP.type_ HP.InputText
    , HP.placeholder "put creator's offer here"
    , HP.required true
    , HE.onValueInput $ Just <<< WriteOffer
    , HE.onKeyDown \e -> if (KE.key e) == "Enter" then Just $ AcceptOffer 0 else Nothing
    ]
  , HH.button [ HE.onClick \_ -> Just $ AcceptOffer 0 ] [ HH.text "Accept Offer" ]
  , HH.button [ HE.onClick \_ -> Just $ CopyToClipboard "answer-text" ] [ HH.text "Copy Answer" ]
  , HH.textarea [ HP.placeholder "answer will appear here", HP.id_ "answer-text", HP.value state.answer ]

  , HH.h1 [] [ HH.text "Joiner 2" ]
  , HH.input
    [ HP.type_ HP.InputText
    , HP.placeholder "put creator's offer here"
    , HP.required true
    , HE.onValueInput $ Just <<< WriteOffer
    , HE.onKeyDown \e -> if (KE.key e) == "Enter" then Just $ AcceptOffer 1 else Nothing
    ]
  , HH.button [ HE.onClick \_ -> Just $ AcceptOffer 1 ] [ HH.text "Accept Offer" ]
  , HH.button [ HE.onClick \_ -> Just $ CopyToClipboard "answer-text-2" ] [ HH.text "Copy Answer" ]
  , HH.textarea [ HP.placeholder "answer will appear here", HP.id_ "answer-text-2", HP.value state.answer ]

  , HH.h1 [] [ HH.text "Chat" ]
  , HH.input
    [ HP.type_ HP.InputText
    , HP.value state.chatInputMessage
    , HP.required true
    , HE.onValueInput $ Just <<< WriteMessage
    , HE.onKeyDown \e -> if (KE.key e) == "Enter" then Just SendMessage else Nothing
    ]
  , HH.button [ HE.onClick \_ -> Just SendMessage ] [ HH.text "Send" ]
  , HH.div_ $ (\m -> HH.p [] [ HH.text m ]) <$> (take 5 state.messages)
  , HH.button [ HE.onClick \_ -> Just $ StartNewGame ] [ HH.text "New Game" ]
  , HH.button [ HE.onClick \_ -> Just $ LoadGame ] [ HH.text "Load Game" ]
  ] <> case state.gameState of
    Nothing -> []
    Just (UpdateState gs) ->
      [ HH.slot (SProxy :: SProxy "Domination") 0 (Domination.component (length gs.players) state.playerIndex) (UpdateState gs) (Just <<< UpdateGameState) ]
    Just (MakeNewGame n) -> [ HH.slot (SProxy :: SProxy "Domination") 0 (Domination.component n state.playerIndex) (MakeNewGame n) (Just <<< UpdateGameState) ]

data AppAction = MakeOffer Int
  | CopyToClipboard String
  | WriteUsername String
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

data Message = ChatMessage String | GameStateMessage GameState
derive instance genericMessage :: Generic Message _
instance encodeJsonMessage :: EncodeJson Message where
  encodeJson = genericEncodeJson
instance decodeJsonMessage :: DecodeJson Message where
  decodeJson = genericDecodeJson
readMessage :: String -> Either String Message
readMessage = lmap show <<< decodeJson <=< jsonParser
writeMessage :: Message -> String
writeMessage = stringify <<< encodeJson

handleAction :: forall m. MonadState AppState m
  => MonadAff m
  => MonadEffect m
  => AppAction -> m Unit
handleAction = case _ of
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
    H.modify_ \state -> state { username = username }
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
    let msg = readMessage $ FFI.detail customEvent
    case msg of
      Left e -> liftEffect $ Console.log e
      Right mt -> case mt of
        GameStateMessage gs -> do
          H.modify_ \state -> state { gameState = Just $ UpdateState gs, messages = "(incoming game state)" : state.messages }
        ChatMessage message -> do
          let remoteMessage = "<- " <> message
          H.modify_ \state -> state { messages = remoteMessage : state.messages }
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
    H.modify_ _ { gameState = Just $ MakeNewGame state.players }
  UpdateGameState gameState -> do
    s <- H.modify_ _ { gameState = Just $ UpdateState gameState }
    sendMessage $ GameStateMessage gameState
    Storage.save "game_state" gameState
  where
    sendChatMessage = do
      s <- H.get
      sendMessage $ ChatMessage s.chatInputMessage
      let localMessage = "-> " <> s.chatInputMessage
      H.modify_ \state -> state { messages = localMessage : state.messages, chatInputMessage = "" }

    sendMessage = liftEffect <<< FFI.say <<< writeMessage

