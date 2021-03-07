module Domination.UI.Domination where

import Prelude

import AppState (Config)
import AppState as AppState
import Data.Array (filter, findIndex, length, (!!), (:))
import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Lens.Fold ((^?))
import Data.Lens.Setter ((%~), (.~))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Domination.Capability.Audio (class Audio, beep)
import Domination.Capability.Audio as Sound
import Domination.Capability.Dom (class Dom)
import Domination.Capability.Log (class Log, error, log)
import Domination.Capability.Random (class Random)
import Domination.Capability.Storage (class Storage)
import Domination.Data.Card (Card)
import Domination.Data.Card (isAction) as Card
import Domination.Data.Choice (Choice(..))
import Domination.Data.GameState (GameState, newGame)
import Domination.Data.GameState as Dom
import Domination.Data.Phase (Phase(..))
import Domination.Data.Play (Play(..))
import Domination.Data.Play as Play
import Domination.Data.Player (Player)
import Domination.Data.Player as Player
import Domination.Data.Reaction (Reaction(..))
import Domination.Data.SelectCards (SelectCards(..))
import Domination.Data.Stack (Stack, stackCards)
import Domination.Data.Supply (negativePoints, positivePoints)
import Domination.UI.Card (render) as Card
import Domination.UI.ChoiceMoveFromTo as MoveFromTo
import Domination.UI.Css as Css
import Domination.UI.DomSlot (Area(..), DomSlot(..))
import Domination.UI.Domination.Action (Action(..))
import Domination.UI.Domination.ActiveState (ActiveState, _i, _playerIndex, _showSupply, _state)
import Domination.UI.Domination.GameEvent (GameEvent(..))
import Domination.UI.Hud as Hud
import Domination.UI.Icons as Icons
import Domination.UI.PickN as PickN
import Domination.UI.RenderText (renderText)
import Domination.UI.Util (acknowledge, chooseOne, h1__)
import Halogen as H
import Halogen.Component (ComponentSlot)
import Halogen.Data.Slot (Slot)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM (HalogenM)
import Web.UIEvent.MouseEvent (MouseEvent)

data GameQuery a
  = LoadActiveState ActiveState a
  | ReceiveGameState { state :: GameState, i :: Int } a
  | StartNewGameRequest AppState.Config a

derive instance genericGameQuery :: Generic (GameQuery a) _

_component :: SProxy "Domination"
_component = SProxy

type Component query r m action =
  ComponentSlot
  HTML
  ("Domination" :: Slot query GameEvent DomSlot | r)
  m
  action

component
  :: forall input m
  . Log m
  => Storage m
  => Dom m
  => Random m
  => Audio m
  => Config
  -> H.Component HTML GameQuery input GameEvent m
component config = H.mkComponent { initialState, render, eval }
  where
  initialState _ =
    { i: 0
    , playerIndex: config.nextPlayerIndex
    , playerCount: config.nextPlayerCount
    , state: newGame 1 kingdom
    , showSupply: true
    }
  kingdom = _.card <$> _.selected `filter` config.kingdom
  render = renderPlayerN
  eval = H.mkEval H.defaultEval
    { handleAction = handleAction
    , handleQuery = handleQuery
    }

updateShowSupply activeState { turn, phase } =
  case activeState, activeState.playerIndex, turn, phase of
  { state: { turn } }, me, nt, BuyPhase
    | me == turn && me /= nt -> false
    | me == nt -> true
  { state: { turn } }, me, nt, ActionPhase
    | me == nt -> false
  { showSupply }, _, _, _ -> showSupply


handleQuery
  :: forall action slots m a
  . Log m
  => Storage m
  => Audio m
  => Random m
  => GameQuery a
  -> H.HalogenM ActiveState action slots GameEvent m (Maybe a)
handleQuery = case _ of
  ReceiveGameState { state, i } a -> do
    activeGame <- H.get

    if Dom.isAttacked activeGame.playerIndex state
    then beep Sound.Attacked
    else pure unit

    log "Domination: UpdateState"
    log "I should ask the user if they want to load this state"
    let
      previousI = activeGame.i
      expectedI = previousI + one
      isExpected = i == expectedI
    log $ "Expected i = " <> (show expectedI)
      <> ". Received i = " <> (show i)

    let
      newShowSupply = updateShowSupply activeGame state
      newActiveState =
        { playerIndex: activeGame.playerIndex
        , playerCount: length state.players
        , state
        , i
        , showSupply: newShowSupply
        }
    H.put newActiveState
    H.raise $ SaveGame newActiveState
    pure $ Just a

  LoadActiveState activeState@{ showSupply, state, playerIndex } a -> do
    activeGame <- H.get
    let
      newShowSupply = updateShowSupply activeGame state

    H.put $ (_showSupply .~ newShowSupply) activeState
    pure $ Just a

  StartNewGameRequest config a -> do
    let { kingdom, nextPlayerIndex: playerIndex, nextPlayerCount: playerCount } = config

    H.modify_ $ _playerIndex .~ playerIndex
    log $ "Domination: StartNewGame as player " <> show playerIndex
    let supply = _.card <$> _.selected `filter` kingdom
    playAndReport playerIndex $ NewGame { playerCount, supply }
    pure $ Just a

type ChildComponents query r m =
  ComponentSlot
  HTML
  ( "MoveFromTo" :: H.Slot query Choice DomSlot
  , "PickN" :: H.Slot query (Array Choice) DomSlot
  , "description" :: H.Slot query Unit DomSlot
  | r
  )
  m
  Action

renderPlayerN
  :: forall query r m
  . Dom m
  => Log m
  => ActiveState
  -> HTML (ChildComponents query r m) Action
renderPlayerN activeState = HH.div
  [ HP.class_ Css.domination ] $
  case activeState.state.result of
    Nothing -> renderPlayers activeState
    Just result -> [ renderText result ]

renderSupply'
  :: forall query r m
  . Dom m
  => Log m
  => ActiveState
  -> Player
  -> HTML (ChildComponents query r m) Action
renderSupply' cs@{ showSupply, state, playerIndex } player =
  HH.ul
    [ HP.classes $
      [ Css.supply
      , if showSupply
        then Css.showing
        else Css.collapsed
      , if state.turn == playerIndex
        && state.phase == BuyPhase
        then Css.active
        else Css.inactive
      ] <> if player.buys < one
        then [ Css.waiting ]
        else []
    ]
    [ HH.ul
      [ HP.class_ Css.stats ]
      [ HH.li
        [ HP.class_ Css.stat ]
        [ HH.text $ "+"
        , renderText $ positivePoints cs.state.supply
        ]
      , HH.li
        [ HP.class_ Css.stat ]
        [ renderText $ negativePoints cs.state.supply
        ]
      ]
    , HH.ul_ $ renderSupply player cs
    ]

renderSupply
  :: forall query r m
  . Dom m
  => Log m
  => Player
  -> ActiveState
  -> Array (HTML (ChildComponents query r m) Action)
renderSupply player { playerIndex, state } =
  renderStackI `mapWithIndex` state.supply
  where
    renderStackI stackIndex = renderStack onClick player (CardSlot SupplyArea stackIndex)
      where
        onClick _ =
          Just $ MakePlay $ Purchase { playerIndex, stackIndex }

renderDeck :: forall widget input. Player -> HTML widget input
renderDeck player = HH.button
  [ HE.onClick (const Nothing), HP.class_ Css.deck ]
  [ HH.ul_
    [ HH.li [] [ HH.text "Discard" ]
    , HH.li [] [ HH.text $ show $ length player.discard ]
    ]
  ]

renderDiscard :: forall widget input. Player -> HTML widget input
renderDiscard player = HH.button
  [ HE.onClick (const Nothing), HP.class_ Css.discard ]
  [ HH.ul_
    [ HH.li [] [ HH.text "Deck" ]
    , HH.li [] [ HH.text $ show $ length player.deck ]
    ]
  ]

renderCard
  :: forall query r m
  . Dom m
  => Log m
  => (MouseEvent -> Maybe Action)
  -> Player
  -> Card
  -> DomSlot
  -> HTML (ChildComponents query r m) Action
renderCard onClick player card slotNumber =
  Card.render onClick extraClasses card slotNumber
  where
    extraClasses = [
      if player.actions > zero && Card.isAction card
      then Css.canPlay
      else Css.cantPlay
    ]

renderStack
  :: forall query r m
  . Dom m
  => Log m
  => (MouseEvent -> Maybe Action)
  -> Player
  -> DomSlot
  -> Stack
  -> HTML (ChildComponents query r m) Action
renderStack onClick player slotNumber stack =
  HH.li [ HP.class_ Css.stack ]
    [ HH.ul_
      [ HH.li
        [ HP.class_ Css.stackCount ]
        [ HH.text $ show stack.count ]
      , HH.li
        [ HP.classes
          [ Css.stackCard
          , if player.buys > zero
            && Player.cash player >= stack.card.cost
            && stack.count > zero
            then Css.canBuy
            else Css.cantBuy
          ]
        ]
        [ renderCard
          onClick
          player
          stack.card
          slotNumber
        ]
      ]
    ]

renderPlayers
  :: forall query r m
  . Dom m
  => Log m
  => ActiveState
  -> Array (HTML (ChildComponents query r m) Action)
renderPlayers cs@{ playerIndex, state } =
  case state.players !! playerIndex of
    Nothing ->
      [ HH.text $ "You cannot be player " <> show (playerIndex + 1)
        <> " in a " <> show (length state.players) <> " player game."
      ]
    Just player -> renderPlayer cs player

renderPlayer
  :: forall query r m
  . Dom m
  => Log m
  => ActiveState
  -> Player
  -> Array (HTML (ChildComponents query r m) Action)
renderPlayer cs@{ state, playerIndex } player =
  [ Hud.render cs
  , if Player.hasChoices player
    && playerIndex == Dom.choiceTurn state
    then HH.div [ HP.class_ Css.dialogue ] $ fromMaybe [] $
      let
        choice = Player.firstChoice player
        hasReaction = Dom.hasReaction playerIndex state
        isAttacked = Dom.isAttacked playerIndex state
      in
      if isAttacked && hasReaction
      then pure <$> renderReaction
      else pure <$> renderChoice (CardSlot ChoiceArea) choice
    else HH.div_ []
  , case state.players !! state.turn of
      Nothing -> h1__ "Something has gone terribly wrong!"
      Just currentPlayer -> HH.div
        ( if state.turn /= playerIndex
          || Dom.choicesOutstanding state
          then [ HP.class_ Css.waiting ]
          else []
        )
        [ HH.div
          [ HP.class_ Css.supplyContainer ]
          [ renderSupply' cs player ]
        , HH.div
          [ HP.class_ Css.table ]
          [ renderAtPlay currentPlayer
          , renderBuying currentPlayer
          ]
        , renderHand player cs
        , Hud.renderHandInfos cs
        , HH.div
          [ HP.class_ Css.controls ]
          [ HH.button
            [ HP.class_ Css.undo
            , HE.onClick \_ -> Just $ UndoRequest cs
            ]
            [ HH.text "Undo" ]
          , HH.button
            [ HP.class_ Css.supplyToggle
            , HE.onClick \_ -> Just ToggleSupply
            ]
            [ HH.text "Supply" ]
          , renderNextPhaseButton cs
          ]
        ]
  ]
    where
      renderReaction
        :: forall widget
        . Maybe (HTML widget Action)
      renderReaction =
        Just $ chooseOne (HH.text "Block attack?")
          [ { clickEvent: MakePlay $
              React { playerIndex, reaction: Just BlockAttack }
            , text: HH.text "Yes"
            }
          , { clickEvent: MakePlay $
              React { playerIndex, reaction: Nothing }
            , text: HH.text "No"
            }
          ]
      renderChoice
        :: (Int -> DomSlot)
        -> Maybe Choice
        -> Maybe (HTML (ChildComponents query r m) Action)
      renderChoice baseSlotNumber = map \choice -> case choice of
          If x ->
            acknowledge
              (renderText choice)
              (playEvent If x unit)
          And x@{ choices } ->
            acknowledge
              (renderText choice)
              (playEvent And x unit)
          Or x@{ choices } ->
            chooseOne (HH.text "Choose one")
              $ choices <#> \choice' ->
                { clickEvent: playEvent Or x choice'
                , text: renderText choice'
                }
          PickN x@{ n, choices } -> HH.div_
            [ HH.slot
              (SProxy :: SProxy "PickN")
              (AreaSlot ChoiceArea)
              ( PickN.component
                { title: "Choose " <> show n
                , n
                , choices
                }
              )
              unit
              $ Just
              <<< MakePlay
              <<< ResolveChoice
              <<< ({ playerIndex, choice: _ })
              <<< PickN
              <<< (x { resolution = _ })
              <<< Just
            ]
          Option x ->
            chooseOne (renderText x.choice)
              [ { clickEvent: MakePlay $ ResolveChoice
                  { playerIndex
                  , choice: Option x { resolution = Just true }
                  }
                , text: HH.text "Yes"
                }
              , { clickEvent: MakePlay $ ResolveChoice
                  { playerIndex
                  , choice: Option x { resolution = Just false }
                  }
                , text: HH.text "No"
                }
              ]
          MoveFromTo _ -> HH.div_
            [ HH.slot
              (SProxy :: SProxy "MoveFromTo")
              (AreaSlot ChoiceArea)
              (MoveFromTo.component player choice baseSlotNumber)
              unit
              $ Just
              <<< MakePlay
              <<< ResolveChoice
              <<< { playerIndex, choice: _ }
            ]
          GainCards x@{ n, cardName } ->
            acknowledge
            (renderText choice)
            (playEvent GainCards x unit)
          GainActions x@{ n } ->
            acknowledge
            (renderText choice)
            (playEvent GainActions x unit)
          GainBuys x@{ n } ->
            acknowledge
            (renderText choice)
            (playEvent GainBuys x unit)
          Discard x@{ selection: SelectAll } ->
            acknowledge (renderText choice) (playEvent Discard x unit)
          Draw x@{ n } ->
            acknowledge
            (renderText choice)
            (playEvent Draw x unit)
          GainBonus x ->
            acknowledge
            (renderText choice)
            (playEvent GainBonus x unit)
        where
          playEvent
            :: forall a r2
            . ({ resolution :: Maybe a | r2 } -> Choice)
            -> { resolution :: Maybe a | r2 }
            -> a
            -> Action
          playEvent mk x r = MakePlay $ ResolveChoice
            { playerIndex
            , choice: mk x { resolution = Just r }
            }

renderNextPhaseButton
  :: forall w
  . ActiveState
  -> HTML w Action
renderNextPhaseButton { playerIndex, state } =
  HH.button
    [ HP.class_ Css.nextPhase
    , HE.onClick $ const $ Just $ MakePlay $ EndPhase { playerIndex }
    ]
    [ HH.span_ if state.turn == playerIndex
      then if Dom.choicesOutstanding state
        then [ HH.text $ "Waiting for Player "
          <> show (Dom.choiceTurn state + one)
          <> " to Choose" ]
        else case state.phase of
          ActionPhase ->
            [ HH.text "Complete "
            , Icons.actions
            , HH.text " Phase"
            ]
          BuyPhase ->
            [ HH.text "Complete "
            , Icons.buys
            , HH.text " Phase"
            ]
          CleanupPhase ->
            pure $ HH.text "Complete Turn"
      else
        [ HH.text $ "Waiting for Player "
          <> show (state.turn + one) <> " | "
        , renderText state.phase
        ]
    ]

renderAtPlay
  :: forall query r m
  . Dom m
  => Log m
  => Player
  -> HTML (ChildComponents query r m) Action
renderAtPlay currentPlayer =
  HH.ul [ HP.class_ Css.play ] $ title : stacks
  where
    title = HH.li
      [ HP.class_ Css.playTitle ]
      [ HH.text "At Play" ]
    stacks = (\i -> renderStack (const Nothing) currentPlayer (CardSlot AtPlayArea i))
      `mapWithIndex` stackCards currentPlayer.atPlay

renderBuying
  :: forall query r m
  . Dom m
  => Log m
  => Player
  -> HTML (ChildComponents query r m) Action
renderBuying currentPlayer =
  HH.ul [ HP.class_ Css.buying ] $ title : stacks
  where
    title = HH.li
      [ HP.class_ Css.buyingTitle ]
      [ HH.text "Buying" ]
    stacks = (\i -> renderStack (const Nothing) currentPlayer (CardSlot BuyingArea i))
      `mapWithIndex` stackCards currentPlayer.buying

renderHand
  :: forall query r m
  . Dom m
  => Log m
  => Player
  -> ActiveState
  -> HTML (ChildComponents query r m) Action
renderHand player { playerIndex, state } = HH.ul
  [ HP.classes $
    [ Css.hand
    , if state.turn == playerIndex
      && state.phase == ActionPhase
      then Css.active
      else Css.inactive
    ] <>
    if player.actions < one
    || (length $ Card.isAction `filter` player.hand) < one
    then [ Css.waiting ]
    else []
  ] $
  title : (\i s -> renderStack (onClick s player.hand) player (CardSlot HandArea i) s)
    `mapWithIndex` (stackCards player.hand)
  where
    title = HH.li
      [ HP.class_ Css.handTitle ]
      [ HH.text "Hand" ]
    onClick stack hand = const $ Just $ MakePlay
      $ PlayCard { playerIndex,  cardIndex }
      where
        cardIndex = fromMaybe (-one)
          $ findIndex (_.name >>> (_ == stack.card.name)) hand

handleAction
  :: forall s p m
  . Log m
  => Random m
  => Audio m
  => Action
  -> HalogenM ActiveState p s GameEvent m Unit
handleAction = case _ of
  MakePlay play -> do
    let playerIndex = fromMaybe zero $ play ^? Play._playerIndex
    log "Domination: MakePlay"
    playAndReport playerIndex play
  UndoRequest as -> H.raise $ Undo as
  ToggleSupply -> H.modify_ $ _showSupply %~ not

playAndReport
  :: forall s p m
  . Log m
  => Random m
  => Audio m
  => Int
  -> Play
  -> HalogenM ActiveState p s GameEvent m Unit
playAndReport playerIndex play = do
  log $ "~Domination: play: " <> show play
  activeState@{ state, showSupply } <- H.get
  let
    lastPhase = state.phase
    lastTurn = state.turn
  result <- Dom.makeAutoPlay play activeState.state
  case result of
    Left e -> do
      beep Sound.Error
      error e
    Right gameState -> do
      case play of
        Purchase _ -> beep Sound.Purchase
        _ -> beep Sound.Acknowledge
      let
        phase = gameState.phase
        turn = gameState.turn
        newShowSupply =
          case play, playerIndex, lastPhase, phase, lastTurn, turn of
          NewGame _, me, _, _, _, t
            | me == t -> true
          _, me, BuyPhase, _, lt, t
            | me == lt && me /= t -> false
          _, me, _, ActionPhase, _, t
            | me == t -> false
          _, _, _, _, _, _ -> showSupply
        playMade = case play of
          EndPhase _ -> Nothing
          _ -> Just
            { play
            , playerIndex
            , state: activeState.state
            }
        newI = case play of
          NewGame _ -> zero
          _ -> activeState.i + one
      log $ "playAndReport.newShowSupply: "
        <> show newShowSupply
      H.modify_ $ (_playerIndex .~ activeState.playerIndex)
        >>> (_i .~ newI)
        >>> (_showSupply .~ newShowSupply)
      H.raise $ NewState
        (((_i .~ newI) >>> (_state .~ gameState)) activeState)
        playMade

