module Domination.UI.Domination
  ( component
  , GameUpdate(..)
  ) where

import Prelude

import Data.Array (filter, length, (!!), (:))
import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Domination.Data.Card (Card)
import Domination.Data.Card (isAction, value) as Card
import Domination.Data.Choice as Choice
import Domination.Data.GameState (GameState)
import Domination.Data.GameState as Dom
import Domination.Data.Phase (Phase(..))
import Domination.Data.Play (Play(..))
import Domination.Data.Player (Player)
import Domination.Data.Player as Player
import Domination.Data.Stack (Stack)
import Domination.UI.Card (render) as Card
import Domination.UI.ChoiceDiscardDownTo as Discard
import Domination.UI.ChoiceTrashUpTo as Trash
import Domination.UI.Css as Css
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Halogen (Component)
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM (HalogenM)
import Web.UIEvent.MouseEvent (MouseEvent)

data GameUpdate = UpdateState GameState | MakeNewGame Int

derive instance genericGameUpdate :: Generic GameUpdate _
instance showGameUpdate :: Show GameUpdate where
  show = genericShow

data GameAction = Receive GameUpdate | MakePlay Play

derive instance genericGameAction :: Generic GameAction _
instance showGameAction :: Show GameAction where
  show = genericShow

component :: forall query m. MonadEffect m => Int -> Int -> Component HTML query GameUpdate GameState m
component playerCount playerIndex = H.mkComponent { initialState, render, eval }
  where
  initialState _ = Dom.newGame playerCount
  render = renderPlayerN playerIndex
  eval = H.mkEval H.defaultEval
    { initialize = Just $ Receive $ MakeNewGame playerCount
    , handleAction = handleAction
    , receive = Just <<< Receive
    }

type ChildComponents t1 t2 t3 =
  H.ComponentSlot HTML ("DiscardDownTo" :: H.Slot t1 Choice.Choice Int, "TrashUpTo" :: H.Slot t1 Choice.Choice Int | t2) t3 GameAction

renderPlayerN :: forall t1 t2 t3.
  Int ->
  GameState ->
  HTML (ChildComponents t1 t2 t3) GameAction
renderPlayerN playerIndex state = HH.div_
  [ HH.h1 [] [ HH.text "Domination" ]
  , HH.div_ $ renderPlayers playerIndex state
  ]

renderSupply :: forall a. Int -> Player -> GameState -> Array (HTML a GameAction)
renderSupply playerIndex player state =
  renderStack playerIndex player `mapWithIndex` state.supply

renderDeck :: forall a. Player -> HTML a GameAction
renderDeck player = HH.button
  [ HE.onClick (const Nothing), HP.class_ Css.deck ]
  [ HH.ul_
    [ HH.li [] [ HH.text "Discard" ]
    , HH.li [] [ HH.text $ show ((length player.discard) :: Int) ]
    ]
  ]

renderDiscard :: forall a. Player -> HTML a GameAction
renderDiscard player = HH.button
  [ HE.onClick (const Nothing), HP.class_ Css.discard ]
  [ HH.ul_
    [ HH.li [] [ HH.text "Deck" ]
    , HH.li [] [ HH.text $ show ((length player.deck) :: Int) ]
    ]
  ]

renderCard :: forall a. (MouseEvent -> Maybe GameAction) -> Player -> Card -> HTML a GameAction
renderCard onClick player card =
  Card.render onClick extraClasses card
  where
    extraClasses = [
      if player.actions > 0 && Card.isAction card
      then Css.canPlay
      else Css.cantPlay
    ]

renderStack :: forall a. Int -> Player -> Int -> Stack -> HTML a GameAction
renderStack playerIndex player stackIndex stack =
  HH.li [ HP.class_ Css.stack ]
    [ HH.ul_
      [ HH.li
        [ HP.class_ Css.stackCount ]
        [ HH.text $ show stack.count ]
      , HH.li
        [ HP.classes
          [ Css.stackCard
          , if player.buys > 0 && Player.cash player >= stack.card.cost && stack.count > 0
            then Css.canBuy
            else Css.cantBuy
          ]
        ]
        [ renderCard (\_ -> Just $ MakePlay $ Purchase playerIndex stackIndex) player stack.card ]
      ]
    ]

renderPlayers :: forall t1 t2 t3. Int -> GameState -> Array (HTML (ChildComponents t1 t2 t3) GameAction)
renderPlayers i state = case state.players !! i of
  Nothing -> []
  Just player -> [ renderPlayer state i player ]

playerStats :: forall a. GameState -> Int -> Player -> (HTML a GameAction)
playerStats state playerIndex player = HH.li
  [ HP.class_ Css.stat ]
  [ HH.text $ "Player " <> show playerIndex
    <> " | Actions: " <> show player.actions
    <> " | Buys: " <> show player.buys
    <> " | $"
    <> if state.turn == playerIndex && state.phase == BuyPhase
       then show (Player.cash player)
       else if state.turn == playerIndex
       then show (Card.value player.atPlay)
       else "_"
    <> " | VP: " <> show (Player.score player)
    <> (if state.turn == playerIndex then " | " <> renderText state.phase else "")
  ]

renderPlayer
  :: forall t1 t2 t3
  . GameState
  -> Int
  -> Player
  -> HTML (ChildComponents t1 t2 t3) GameAction
renderPlayer state playerIndex player =
  if Player.hasChoices player && playerIndex == Dom.choiceTurn state
  then fromMaybe (HH.div_ []) $
    let choice = (Player.firstChoice player) in
    choice <#> case _ of
      Choice.TrashUpTo _ _ -> HH.div_
        [ (HH.slot (SProxy :: SProxy "TrashUpTo") 0 (Trash.component player) unit (Just <<< MakePlay <<< ResolveChoice playerIndex)) ]
      Choice.DiscardDownTo _ _ -> HH.div_
        [ (HH.slot (SProxy :: SProxy "DiscardDownTo") 1 (Discard.component player) unit (Just <<< MakePlay <<< ResolveChoice playerIndex)) ]
  else
  case state.players !! state.turn of
    Nothing -> HH.h1_ [ HH.text "Something has gone terribly wrong!" ]
    Just currentPlayer -> HH.div
      ( if state.turn /= playerIndex
        then [ HP.class_ Css.waiting ]
        else []
      )
      [ HH.h2 [] [ HH.text $ "Player " <> show playerIndex ]
      , HH.ul
          [ HP.classes $
            [ Css.supply
            , if state.turn == playerIndex && state.phase == BuyPhase
              then Css.active
              else Css.inactive
            ] <> if player.buys < 1
              then [ Css.waiting ]
              else []
          ]
          $ HH.li_ [(HH.h3 [] [ HH.text $ "Supply" ])]
            : HH.ul_
              [ HH.li
                [ HP.class_ Css.handInfo ]
                [ HH.text $ "$" <> (show ((Player.cash player) :: Int)) ]
              , HH.li
                [ HP.class_ Css.handInfo ]
                [ HH.text $ (show $ player.buys) <> " Buys" ]
              ]
            : [ HH.ul_ (renderSupply playerIndex player state) ]
      , HH.button
        [ HP.class_ (Css.nextPhase), HE.onClick \_ -> Just $ MakePlay $ EndPhase playerIndex ]
        [ HH.text if state.turn == playerIndex
          then case state.phase of
            ActionPhase -> "Complete Action Phase"
            BuyPhase -> "Complete Buy Phase"
            CleanupPhase -> "Complete Turn"
          else "Waiting for Player " <> show state.turn <> " | " <> renderText state.phase
        ]
      , HH.ul
        [ HP.class_ Css.stats ]
        (playerStats state `mapWithIndex` state.players)
      , HH.ul
        [ HP.class_ Css.play ]
        (HH.li_ [(HH.h3 [] [ HH.text $ "Play" ])]
        : (renderCard (const Nothing) currentPlayer <$> currentPlayer.atPlay))
      , HH.ul
        [ HP.class_ Css.buying ]
        (HH.li_ [(HH.h3 [] [ HH.text $ "Buying" ])]
        : (renderCard (const Nothing) currentPlayer <$> currentPlayer.buying))
      , renderHand player playerIndex state
      ]

renderHand :: forall a. Player -> Int -> GameState -> HTML a GameAction
renderHand player playerIndex state = HH.ul
  [ HP.classes $
    [ Css.hand
    , if state.turn == playerIndex && state.phase == ActionPhase
      then Css.active
      else Css.inactive
    ] <> if player.actions < 1 || (length $ Card.isAction `filter` player.hand) < 1
      then [ Css.waiting ]
      else []
  ] $
  [ HH.li_ [ HH.h3 [] [ HH.text $ "Hand" ] ]
  , HH.li_
    [ HH.ul_
      [ HH.li [ HP.class_ Css.handInfo ] [ HH.text $ (show $ player.actions) <> " Actions" ]
      , HH.li [ HP.class_ Css.handInfo ] [ HH.text $ "$" <> (show ((Player.cash player) :: Int)) ]
      , HH.li [ HP.class_ Css.handInfo ] [ HH.text $ (show $ player.buys) <> " Buys" ]
      ]
    ]
  , renderDiscard player
  ] <> renderCardInHand player playerIndex `mapWithIndex` player.hand
  <> [ renderDeck player ]

renderCardInHand :: forall a. Player -> Int -> Int -> Card -> HTML a GameAction
renderCardInHand player playerIndex cardIndex card =
  renderCard (\_ -> Just $ MakePlay $ PlayCard playerIndex cardIndex) player card

handleAction :: forall s m. MonadEffect m => GameAction -> HalogenM GameState GameAction s GameState m Unit
handleAction = case _ of
  Receive update -> case update of
    MakeNewGame n -> playAndReport $ NewGame n
    UpdateState gs -> H.put gs
  MakePlay play -> playAndReport play
  where
    playAndReport play = do
      result <- Dom.makeAutoPlay play
      case result of
        Left e -> do
          liftEffect $ Console.error e
          H.get >>= H.raise
        Right _ -> H.get >>= H.raise

class RenderText a where
  renderText :: a -> String

instance phaseRenderText :: RenderText Phase where
  renderText ActionPhase = "Action Phase"
  renderText BuyPhase = "Buy Phase"
  renderText CleanupPhase = "Cleanup Phase"

