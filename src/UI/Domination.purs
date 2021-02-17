module Domination.UI.Domination where

import Prelude

import Data.Array (catMaybes, filter, findIndex, head, length, nub, reverse, (!!), (:))
import Data.Either (Either(..), hush)
import Data.Foldable (foldr)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Lens.Fold ((^?))
import Data.Lens.Lens (Lens', Lens)
import Data.Lens.Prism.Maybe (_Just)
import Data.Lens.Record (prop)
import Data.Lens.Setter ((%~), (.~))
import Data.Lens.Traversal (Traversal')
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Domination.Capability.Dom (class Dom)
import Domination.Capability.Log (class Log, error, log)
import Domination.Capability.Random (class Random)
import Domination.Capability.Storage (class Storage, load, save)
import Domination.Data.Card (Card)
import Domination.Data.Card (isAction) as Card
import Domination.Data.Choice (Choice(..))
import Domination.Data.GameState (GameState)
import Domination.Data.GameState as Dom
import Domination.Data.Phase (Phase(..))
import Domination.Data.Play (Play(..))
import Domination.Data.Play as Play
import Domination.Data.Player (Player)
import Domination.Data.Player as Player
import Domination.Data.Reaction (Reaction(..))
import Domination.Data.SelectCards (SelectCards(..))
import Domination.Data.Stack (Stack)
import Domination.UI.Card (render) as Card
import Domination.UI.ChoiceMoveFromTo as MoveFromTo
import Domination.UI.Css as Css
import Domination.UI.DomSlot (Area(..), DomSlot(..))
import Domination.UI.Hud as Hud
import Domination.UI.Icons as Icons
import Domination.UI.PickN as PickN
import Domination.UI.RenderText (renderText)
import Domination.UI.Util (acknowledge, chooseOne, h1__)
import Domination.UI.Util as Util
import Halogen as H
import Halogen.Component (ComponentSlot)
import Halogen.Data.Slot (Slot)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM (HalogenM)
import Web.UIEvent.MouseEvent (MouseEvent)

data InternalGameAction
  = WritePlayerIndex Int
  | WritePlayerCount Int
  | StartNewGame
  | MakePlay Play
  | LoadGameRequest
  | UndoRequest ActiveState
  | ToggleSupply
  | ToggleMenu

data GameEvent
  = NewState
    ActiveState
    (Maybe { play :: Play, playerIndex :: Int, state :: GameState })
  | LoadGame
  | SaveGame ActiveState
  | Undo ActiveState

data GameQuery a
  = LoadActiveState ActiveState a
  | ReceiveGameState { state :: GameState, i :: Int } a

derive instance genericGameQuery :: Generic (GameQuery a) _

type ActiveState =
  { i :: Int
  , playerIndex :: Int
  , playerCount :: Int
  , state :: GameState
  , showSupply :: Boolean
  }

_i
  :: forall a b r
  . Lens { i :: a | r } { i :: b | r } a b
_i = prop (SProxy :: SProxy "i")

_state'
  :: forall a b r
  . Lens { state :: a | r } { state :: b | r } a b
_state' = prop (SProxy :: SProxy "state")

type ComponentState =
  { nextPlayerIndex :: Int
  , nextPlayerCount :: Int
  , maybeGame :: Maybe ActiveState
  , showMenu :: Boolean
  }

_nextPlayerIndex :: Lens' ComponentState Int
_nextPlayerIndex = prop (SProxy :: SProxy "nextPlayerIndex")
_nextPlayerCount :: Lens' ComponentState Int
_nextPlayerCount = prop (SProxy :: SProxy "nextPlayerCount")
_playerIndex :: Lens' ActiveState Int
_playerIndex = prop (SProxy :: SProxy "playerIndex")
_playerCount :: Lens' ActiveState Int
_playerCount = prop (SProxy :: SProxy "playerCount")
_maybeGame :: Lens' ComponentState (Maybe ActiveState)
_maybeGame = prop (SProxy :: SProxy "maybeGame")
_state :: Traversal' ComponentState GameState
_state = _maybeGame <<< _Just <<< prop (SProxy :: SProxy "state")
_showSupply
  :: forall a b r
  . Lens { showSupply :: a | r } { showSupply :: b | r } a b
_showSupply = prop (SProxy :: SProxy "showSupply")
_showMenu
  :: forall a b r
  . Lens { showMenu :: a | r } { showMenu :: b | r } a b
_showMenu = prop (SProxy :: SProxy "showMenu")

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
  => H.Component HTML GameQuery input GameEvent m
component = H.mkComponent { initialState, render, eval }
  where
  initialState _ =
    { nextPlayerCount: one
    , nextPlayerIndex: zero
    , maybeGame: Nothing
    , showMenu: false
    }
  render = renderPlayerN
  eval = H.mkEval H.defaultEval
    { handleAction = handleAction
    , handleQuery = handleQuery
    }

handleQuery
  :: forall action slots m a
  . Log m
  => Storage m
  => Random m
  => GameQuery a
  -> H.HalogenM ComponentState action slots GameEvent m (Maybe a)
handleQuery = case _ of
  ReceiveGameState { state, i } a -> do
    cs <- H.get
    log "Domination: UpdateState"
    ok <- case cs.maybeGame of
      Nothing -> pure true
      Just activeGame -> do
        log "I should ask the user if they want to load this state"
        let
          previousI = activeGame.i
          expectedI = previousI + one
          isExpected = i == expectedI
        log
          $ "Expected i = " <> show expectedI
          <> ". Received i = " <> show i
        pure isExpected
    let
      newShowSupply =
        case cs.maybeGame, cs.nextPlayerIndex, state.turn, state.phase of
          Nothing, me, nt, BuyPhase | me == nt -> true
          Nothing, me, nt, _ -> me /= nt
          Just { state: { turn } }, me, nt, BuyPhase
            | me == turn && me /= nt -> false
            | me == nt -> true
          Just { state: { turn } }, me, nt, ActionPhase
            | me == nt -> false
          Just { showSupply }, _, _, _ -> showSupply
      newActiveState =
        { playerIndex: cs.nextPlayerIndex
        , playerCount: length state.players
        , state
        , i
        , showSupply: newShowSupply
        }
    H.modify_ $ _maybeGame .~ (Just newActiveState)
    H.raise $ SaveGame newActiveState
    pure $ Just a
  LoadActiveState activeState@{ showSupply, state, playerIndex } a -> do
    { nextPlayerIndex, nextPlayerCount, maybeGame } <- H.get

    loadedPlayerIndex <- load "player_index"
    loadedPlayerCount <- load "player_count"
    log $ "loaded player_index: " <> show loadedPlayerIndex
    log $ "loaded player_count: " <> show loadedPlayerCount
    let
      newPlayerIndex = fromMaybe nextPlayerIndex
        $ hush loadedPlayerIndex
      newPlayerCount = fromMaybe nextPlayerCount
        $ hush loadedPlayerCount

    let
      newShowSupply =
        case maybeGame, playerIndex, state.turn, state.phase of
          Nothing, me, nt, BuyPhase | me == nt -> true
          Nothing, me, nt, _ -> me /= nt
          Just { state: { turn } }, me, nt, BuyPhase
            | me == turn && me /= nt -> false
            | me == nt -> true
          Just { state: { turn } }, me, nt, ActionPhase
            | me == nt -> false
          Just { showSupply }, _, _, _ -> showSupply
      newActiveState =
        ( (_playerIndex .~ newPlayerIndex)
        >>> (_playerCount .~ newPlayerCount)
        >>> (_showSupply .~ newShowSupply)
        ) activeState
    log $ "LoadActiveState as player " <> show newPlayerIndex
    log $ "playerIndex:" <> show playerIndex
      <> "\nlastTurn:"
      <> (fromMaybe "???" (show <<< (_.state.turn) <$> maybeGame))
      <> "\nnextTurn:" <> show state.turn
      <> "\nnextPhase:" <> show state.phase
    log $ "LoadActiveState.newShowSupply: " <> show newShowSupply

    H.modify_ $ (_maybeGame .~ Just newActiveState)
      >>> (_nextPlayerIndex .~ newPlayerIndex)
      >>> (_nextPlayerCount .~ newPlayerCount)
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
  InternalGameAction

renderPlayerN
  :: forall query r m
  . Dom m
  => Log m
  => ComponentState
  -> HTML (ChildComponents query r m) InternalGameAction
renderPlayerN cs@{ nextPlayerIndex, nextPlayerCount } = HH.div
  [ HP.class_ Css.domination ]
  [ HH.div
    []
    [ HH.button
      [ HP.class_ Css.settingsButton
      , HE.onClick \_ -> Just ToggleMenu
      ]
      [ HH.text "Settings" ]
    ]
  , HH.div
    [ HP.classes
      [ Css.settingsMenu
      , if cs.showMenu
        then Css.showing
        else Css.collapsed
      ]
    ]
    [ Util.incrementer
      { label: "Players: "
      , mbMin: Just one
      , mbMax: Nothing
      , value: nextPlayerCount
      , setValue: WritePlayerCount
      }
    , Util.incrementer
      { label: "Player #: "
      , mbMin: Just one
      , mbMax: Nothing
      , value: nextPlayerIndex + one
      , setValue: (_ - one) >>> WritePlayerIndex
      }
    , HH.div_
      [ HH.button
        [ HE.onClick \_ -> Just $ StartNewGame ]
        [ HH.text $ "Start New " <> show nextPlayerCount
          <> " Player Game as Player " <> show (nextPlayerIndex + one)
        ]
      , HH.button
        [ HE.onClick \_ -> Just $ LoadGameRequest ]
        [ HH.text "Load Game" ]
      ]
    ]
    , case cs.maybeGame of
      Nothing -> HH.div_ []
      Just activeState ->
        HH.div_ [ HH.div_ $ renderPlayers activeState ]
  ]

renderSupply'
  :: forall query r m
  . Dom m
  => Log m
  => ActiveState
  -> Player
  -> HTML (ChildComponents query r m) InternalGameAction
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
    [ HH.ul_ $ renderSupply player cs
    ]

renderSupply
  :: forall query r m
  . Dom m
  => Log m
  => Player
  -> ActiveState
  -> Array (HTML (ChildComponents query r m) InternalGameAction)
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
  => (MouseEvent -> Maybe InternalGameAction)
  -> Player
  -> Card
  -> DomSlot
  -> HTML (ChildComponents query r m) InternalGameAction
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
  => (MouseEvent -> Maybe InternalGameAction)
  -> Player
  -> DomSlot
  -> Stack
  -> HTML (ChildComponents query r m) InternalGameAction
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
  -> Array (HTML (ChildComponents query r m) InternalGameAction)
renderPlayers cs@{ playerIndex, state } =
  case state.players !! playerIndex of
    Nothing -> []
    Just player -> [ renderPlayer cs player ]

renderPlayer
  :: forall query r m
  . Dom m
  => Log m
  => ActiveState
  -> Player
  -> HTML (ChildComponents query r m) InternalGameAction
renderPlayer cs@{ state, playerIndex } player = HH.div_
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
  ,   case state.players !! state.turn of
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
          , renderAtPlay currentPlayer
          , renderBuying currentPlayer
          , renderHand player cs
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
        . Maybe (HTML widget InternalGameAction)
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
        -> Maybe (HTML (ChildComponents query r m) InternalGameAction)
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
            -> InternalGameAction
          playEvent mk x r = MakePlay $ ResolveChoice
            { playerIndex
            , choice: mk x { resolution = Just r }
            }

renderNextPhaseButton
  :: forall w
  . ActiveState
  -> HTML w InternalGameAction
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
  -> HTML (ChildComponents query r m) InternalGameAction
renderAtPlay currentPlayer =
  HH.ul [ HP.class_ Css.play ] $ title : stacks
  where
    title = HH.li
      [ HP.class_ Css.playTitle ]
      [ HH.text "At Play" ]
    stacks = (\i -> renderStack (const Nothing) currentPlayer (CardSlot AtPlayArea i))
      `mapWithIndex` stackCards currentPlayer.atPlay

stackCards :: Array Card -> Array Stack
stackCards cards = catMaybes (foldr f [] names)
  where
    names = nub $ _.name <$> reverse cards
    f name stacks =
      ({ card: _, count: length cards' } <$> head cards')
        : stacks
      where
        cards' = (_.name >>> (_ == name)) `filter` cards

renderBuying
  :: forall query r m
  . Dom m
  => Log m
  => Player
  -> HTML (ChildComponents query r m) InternalGameAction
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
  -> HTML (ChildComponents query r m) InternalGameAction
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
  => Storage m
  => Random m
  => InternalGameAction
  -> HalogenM ComponentState p s GameEvent m Unit
handleAction = case _ of
  WritePlayerIndex index -> do
    { nextPlayerIndex, nextPlayerCount } <- H.get
    log $ "Domination: updating playerIndex from "
      <> show nextPlayerIndex <> " -> " <> show index
    let
      newPlayerIndex = max index zero
      newPlayerCount = max (index + one) nextPlayerCount
    H.modify_ $ (_nextPlayerIndex .~ newPlayerIndex)
      >>> (_nextPlayerCount .~ newPlayerCount)
      >>> (_maybeGame <<< _Just <<< _playerIndex .~ newPlayerIndex)
    log $ "saving player_index: " <> show newPlayerIndex
    save "player_index" newPlayerIndex
    log $ "saving player_count: " <> show newPlayerCount
    save "player_count" newPlayerCount
  WritePlayerCount count -> do
    { nextPlayerIndex, nextPlayerCount } <- H.get
    log $ "Domination: updating playerCount from "
      <> show nextPlayerCount <> " -> " <> show count
    let
      newPlayerIndex = min (count - one) nextPlayerIndex
      newPlayerCount = max count one
    H.modify_ $ (_nextPlayerCount .~ newPlayerCount)
      >>> (_nextPlayerIndex .~ newPlayerIndex)
      >>> (_maybeGame <<< _Just <<< _playerIndex .~ newPlayerIndex)
    log $ "saving player_index: " <> show newPlayerIndex
    save "player_index" newPlayerIndex
    log $ "saving player_count: " <> show newPlayerCount
    save "player_count" newPlayerCount
  StartNewGame -> do
    { nextPlayerIndex: playerIndex, nextPlayerCount: playerCount } <- H.get
    log $ "Domination: StartNewGame as player " <> show playerIndex
    playAndReport playerIndex $ NewGame { playerCount }
  MakePlay play -> do
    let playerIndex = fromMaybe zero $ play ^? Play._playerIndex
    log "Domination: MakePlay"
    playAndReport playerIndex play
  LoadGameRequest -> H.raise LoadGame
  UndoRequest as -> H.raise $ Undo as
  ToggleSupply -> H.modify_
    $ (_maybeGame <<< _Just <<< _showSupply %~ not)
    >>> (_showMenu .~ false)
  ToggleMenu -> H.modify_ $ _showMenu %~ not

playAndReport
  :: forall s p m
  . Log m
  => Random m
  => Int
  -> Play
  -> HalogenM ComponentState p s GameEvent m Unit
playAndReport playerIndex play = do
  log $ "~Domination: play: " <> show play
  { maybeGame, nextPlayerCount, nextPlayerIndex } <- H.get
  case maybeGame of
    Nothing -> case play of
      NewGame { playerCount } -> doTheRest
      -- TODO: WritePlayerCount here
        { i: zero
        , state: Dom.newGame playerCount
        , playerCount: nextPlayerCount
        , playerIndex: nextPlayerIndex
        , showSupply: false
        }
      _ -> error "Domination: playing with no game state?"
    Just activeState -> doTheRest activeState
  where
    doTheRest activeState@{ state, showSupply } = do
      let
        lastPhase = state.phase
        lastTurn = state.turn
      result <- Dom.makeAutoPlay play activeState.state
      case result of
        Left e -> error e
        Right gameState -> do
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
          H.modify_
            $ (_state .~ gameState)
            >>> (_maybeGame <<< _Just
              <<< _playerIndex .~ activeState.playerIndex)
            >>> (_maybeGame <<< _Just
              <<< _i .~ newI)
            >>> (_maybeGame <<< _Just
              <<< _showSupply .~ newShowSupply)
            >>> (_showMenu .~ false)
          H.raise $ NewState
            (((_i .~ newI) >>> (_state' .~ gameState)) activeState)
            playMade

