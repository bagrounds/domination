module Domination.UI.Domination where

import Prelude

import Data.Array (catMaybes, filter, head, length, nub, reverse, (!!), (:))
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Lens.Fold ((^?))
import Data.Lens.Lens (Lens')
import Data.Lens.Prism.Maybe (_Just)
import Data.Lens.Record (prop)
import Data.Lens.Setter ((.~))
import Data.Lens.Traversal (Traversal')
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Domination.Capability.Dom (class Dom)
import Domination.Capability.Log (class Log, error, log)
import Domination.Capability.Random (class Random)
import Domination.Data.Card (Card)
import Domination.Data.Card (isAction, value) as Card
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
import Domination.UI.PickN as PickN
import Domination.UI.RenderText (renderText)
import Domination.UI.Util (acknowledge, h1__, h2__, h3__, chooseOne)
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
  }

type ComponentState =
  { nextPlayerIndex :: Int
  , nextPlayerCount :: Int
  , maybeGame :: Maybe ActiveState
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

_component :: SProxy "Domination"
_component = SProxy

type Component query r m action =
  ComponentSlot
  HTML
  ("Domination" :: Slot query GameEvent Int | r)
  m
  action

component
  :: forall input m
  . Log m
  => Dom m
  => Random m
  => H.Component HTML GameQuery input GameEvent m
component = H.mkComponent { initialState, render, eval }
  where
  initialState _ =
    { nextPlayerCount: 1
    , nextPlayerIndex: 0
    , maybeGame: Nothing
    }
  render = renderPlayerN
  eval = H.mkEval H.defaultEval
    { handleAction = handleAction
    , handleQuery = handleQuery
    }

handleQuery
  :: forall action slots m a
  . Log m
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
          expectedI = previousI + 1
          isExpected = i == expectedI
        log
          $ "Expected i = " <> show expectedI
          <> ". Received i = " <> show i
        pure isExpected
    let
      newActiveState =
        { playerIndex: cs.nextPlayerIndex
        , playerCount: length state.players
        , state
        , i
        }
    H.modify_ $ _maybeGame .~ (Just newActiveState)
    H.raise $ SaveGame newActiveState
    pure $ Just a
  LoadActiveState activeState a -> do
    log $ "LoadActiveState as player " <> show activeState.playerIndex
    H.modify_
      $ (_maybeGame .~ (Just activeState))
      >>> (_nextPlayerIndex .~ activeState.playerIndex)
      >>> (_nextPlayerCount .~ activeState.playerCount)
    pure $ Just a

type ChildComponents query r m =
  ComponentSlot
  HTML
  ( "MoveFromTo" :: H.Slot query Choice Int
  , "PickN" :: H.Slot query (Array Choice) Int
  , "description" :: H.Slot query Unit Int
  | r
  )
  m
  InternalGameAction

renderPlayerN
  :: forall query r m
  . Dom m
  => ComponentState
  -> HTML (ChildComponents query r m) InternalGameAction
renderPlayerN cs@{ nextPlayerIndex, nextPlayerCount } = HH.div_ $
  [ Util.incrementer
    { label: "Players: "
    , mbMin: (Just 1)
    , mbMax: Nothing
    , value: nextPlayerCount
    , setValue: WritePlayerCount
    }
  , Util.incrementer
    { label: "Player #: "
    , mbMin: (Just 1)
    , mbMax: Nothing
    , value: nextPlayerIndex + 1
    , setValue: (_ - 1) >>> WritePlayerIndex
    }
  , HH.div_
    [ HH.button
      [ HE.onClick \_ -> Just $ StartNewGame ]
      [ HH.text $ "Start New " <> show nextPlayerCount
        <> " Player Game as Player " <> show (nextPlayerIndex + 1)
      ]
    , HH.button
      [ HE.onClick \_ -> Just $ LoadGameRequest ]
      [ HH.text "Load Game" ]
    , HH.button
      [ HE.onClick \_ -> x]
      [ HH.text "Undo" ]
    ]
    , case cs.maybeGame of
      Nothing -> HH.div_ []
      Just activeState -> HH.div_
        [ h1__ $ "Domination (" <> show activeState.i <> ")"
        , HH.div_ $ renderPlayers activeState
        ]
  ]
  where
    x = case cs.maybeGame of
      Just activeState -> Just $ UndoRequest activeState
      Nothing -> Nothing

renderSupply'
  :: forall query r m
  . Dom m
  => ActiveState
  -> Player
  -> HTML (ChildComponents query r m) InternalGameAction
renderSupply' cs@{ state, playerIndex } player =
  HH.ul
    [ HP.classes $
      [ Css.supply
      , if state.turn == playerIndex
        && state.phase == BuyPhase
        then Css.active
        else Css.inactive
      ] <> if player.buys < 1
        then [ Css.waiting ]
        else []
    ]
    $ HH.li_ [ h3__ "Supply" ]
      : HH.ul_
        [ HH.li
          [ HP.class_ Css.handInfo ]
          [ HH.text $ "$" <> (show $ Player.cash player) ]
        , HH.li
          [ HP.class_ Css.handInfo ]
          [ HH.text $ (show $ player.buys) <> " Buys" ]
        ]
      : [ HH.ul_ (renderSupply player cs) ]

--renderSupply
--  :: forall widget
--  . Player
--  -> ActiveState
--  -> Array (HTML widget InternalGameAction)
renderSupply player { playerIndex, state } =
  renderStackI `mapWithIndex` state.supply
  where
    renderStackI stackIndex = renderStack onClick player
      where
        onClick _ =
          Just $ MakePlay $ Purchase { playerIndex,  stackIndex }

renderDeck :: forall widget input. Player -> HTML widget input
renderDeck player = HH.button
  [ HE.onClick (const Nothing), HP.class_ Css.deck ]
  [ HH.ul_
    [ HH.li [] [ HH.text "Discard" ]
    , HH.li [] [ HH.text $ show ((length player.discard) :: Int) ]
    ]
  ]

renderDiscard :: forall widget input. Player -> HTML widget input
renderDiscard player = HH.button
  [ HE.onClick (const Nothing), HP.class_ Css.discard ]
  [ HH.ul_
    [ HH.li [] [ HH.text "Deck" ]
    , HH.li [] [ HH.text $ show ((length player.deck) :: Int) ]
    ]
  ]

--renderCard
--  :: forall widget input
--  . (MouseEvent -> Maybe input)
--  -> Player
--  -> Card
--  -> HTML widget input
renderCard onClick player card =
  Card.render onClick extraClasses card
  where
    extraClasses = [
      if player.actions > 0 && Card.isAction card
      then Css.canPlay
      else Css.cantPlay
    ]

--renderStack
--  :: forall widget input
--  . (MouseEvent -> Maybe input)
--  -> Player
--  -> Stack
--  -> HTML widget input
renderStack onClick player stack =
  HH.li [ HP.class_ Css.stack ]
    [ HH.ul_
      [ HH.li
        [ HP.class_ Css.stackCount ]
        [ HH.text $ show stack.count ]
      , HH.li
        [ HP.classes
          [ Css.stackCard
          , if player.buys > 0
            && Player.cash player >= stack.card.cost
            && stack.count > 0
            then Css.canBuy
            else Css.cantBuy
          ]
        ]
        [ renderCard
          onClick
          player
          stack.card
        ]
      ]
    ]

renderPlayers
  :: forall query r m
  . Dom m
  => ActiveState
  -> Array (HTML (ChildComponents query r m) InternalGameAction)
renderPlayers cs@{ playerIndex, state } =
  case state.players !! playerIndex of
    Nothing -> []
    Just player -> [ renderPlayer cs player ]

playerStats
  :: forall a i
  . ActiveState
  -> Int
  -> Player
  -> HTML a i
playerStats { state, playerIndex: me } playerIndex player = HH.li
  [ HP.class_ Css.stat ]
  [ HH.text $ "Player " <> show (playerIndex + 1)
    <> " | Actions: " <> show player.actions
    <> " | Buys: " <> show player.buys
    <> " | $"
    <>
    ( if state.turn == playerIndex
      && state.phase == BuyPhase
      then show (Player.cash player)
      else if state.turn == playerIndex
      then show (Card.value player.atPlay)
      else "_"
    )
    <> " | VP: " <> show (Player.score player)
    <>
    ( if state.turn == playerIndex
      then " | " <> renderText state.phase
      else ""
    )
  ]

renderPlayer
  :: forall query r m
  . Dom m
  => ActiveState
  -> Player
  -> HTML (ChildComponents query r m) InternalGameAction
renderPlayer cs@{ state, playerIndex } player =
  if Player.hasChoices player
  && playerIndex == Dom.choiceTurn state
  then fromMaybe (HH.div_ []) $
    let
      choice = Player.firstChoice player
      hasReaction = Dom.hasReaction playerIndex state
      isAttacked = Dom.isAttacked playerIndex state
    in
    if isAttacked && hasReaction
    then renderReaction
    else renderChoice choice
  else
    case state.players !! state.turn of
      Nothing -> h1__ "Something has gone terribly wrong!"
      Just currentPlayer -> HH.div
        ( if state.turn /= playerIndex
          || Dom.choicesOutstanding state
          then [ HP.class_ Css.waiting ]
          else []
        )
        [ h2__ $ "Player " <> show (playerIndex + 1)
        , renderSupply' cs player
        , renderNextPhaseButton cs
        , renderStats cs
        , renderAtPlay currentPlayer
        , renderBuying currentPlayer
        , renderHand player cs
        ]
    where
      renderReaction
        :: forall widget
        . Maybe (HTML widget InternalGameAction)
      renderReaction =
        Just $ chooseOne "Block attack?"
          [ { clickEvent: MakePlay $
              React { playerIndex, reaction: Just BlockAttack }
            , text: "Yes"
            }
          , { clickEvent: MakePlay $
              React { playerIndex, reaction: Nothing }
            , text: "No"
            }
          ]
      renderChoice
        :: Maybe Choice
        -> Maybe (HTML (ChildComponents query r m) InternalGameAction)
      renderChoice = map \choice -> case choice of
          If x ->
            acknowledge (renderText choice) (playEvent If x unit)
          And x@{ choices } ->
            acknowledge (renderText choice) (playEvent And x unit)
          Or x@{ choices } ->
            chooseOne "Choose one" $
              choices <#> \choice' ->
                { clickEvent: playEvent Or x choice'
                , text: renderText choice'
                }
          PickN x@{ n, choices } -> HH.div_
            [ HH.slot
              (SProxy :: SProxy "PickN")
              0
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
            chooseOne (renderText x.choice <> "?")
              [ { clickEvent: MakePlay $ ResolveChoice
                  { playerIndex
                  , choice: Option x { resolution = Just true }
                  }
                , text: "Yes"
                }
              , { clickEvent: MakePlay $ ResolveChoice
                  { playerIndex
                  , choice: Option x { resolution = Just false }
                  }
                , text: "No"
                }
              ]
          MoveFromTo _ -> HH.div_
            [ HH.slot
              (SProxy :: SProxy "MoveFromTo")
              0
              (MoveFromTo.component player choice)
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
    [ HP.class_ (Css.nextPhase)
    , HE.onClick $ const $ Just $ MakePlay $ EndPhase { playerIndex }
    ]
    [ HH.text if state.turn == playerIndex
      then if Dom.choicesOutstanding state
        then "Waiting for Player "
          <> show (Dom.choiceTurn state + 1)
          <> " to Choose"
        else case state.phase of
          ActionPhase -> "Complete Action Phase"
          BuyPhase -> "Complete Buy Phase"
          CleanupPhase -> "Complete Turn"
      else "Waiting for Player " <> show (state.turn + 1)
        <> " | " <> renderText state.phase
    ]

renderStats :: forall w i. ActiveState -> HTML w i
renderStats cs = HH.ul
  [ HP.class_ Css.stats ]
  (playerStats cs `mapWithIndex` cs.state.players)

--renderAtPlay :: forall w i. Player -> HTML w i
renderAtPlay currentPlayer =
  HH.ul [ HP.class_ Css.play ] $ title : stacks
  where
    title = HH.li_ [ h3__ "At Play" ]
    stacks = renderStack (const Nothing) currentPlayer
      <$> (stackCards currentPlayer.atPlay)

stackCards :: Array Card -> Array Stack
stackCards cards = catMaybes (foldr f [] names)
  where
    names = nub $ _.name <$> reverse cards
    f name stacks =
      ({ card: _, count: length cards' } <$> head cards')
        : stacks
      where
        cards' = (_.name >>> (_ == name)) `filter` cards

--renderBuying :: forall w i. Player -> HTML w i
renderBuying currentPlayer =
  HH.ul [ HP.class_ Css.buying ] $ title : stacks
  where
    title = HH.li_ [ h3__ "Buying" ]
    stacks = renderStack (const Nothing) currentPlayer
      <$> (stackCards currentPlayer.buying)

--renderHand :: forall w. Player -> ActiveState -> HTML w InternalGameAction
renderHand player { playerIndex, state } = HH.ul
  [ HP.classes $
    [ Css.hand
    , if state.turn == playerIndex
      && state.phase == ActionPhase
      then Css.active
      else Css.inactive
    ] <>
    if player.actions < 1
    || (length $ Card.isAction `filter` player.hand) < 1
    then [ Css.waiting ]
    else []
  ] $
  [ HH.li_ [ h3__ "Hand" ]
  , HH.li_
    [ HH.ul_
      [ HH.li
        [ HP.class_ Css.handInfo ]
        [ HH.text $ (show $ player.actions) <> " Actions" ]
      , HH.li
        [ HP.class_ Css.handInfo ]
        [ HH.text $ "$" <> (show $ Player.cash player) ]
      , HH.li
        [ HP.class_ Css.handInfo ]
        [ HH.text $ (show $ player.buys) <> " Buys" ]
      ]
    ]
  , renderDiscard player
  ]
  <> renderCardInHand player playerIndex `mapWithIndex` player.hand
  <> [ renderDeck player ]

--renderCardInHand
--  :: forall a
--  . Player
--  -> Int
--  -> Int
--  -> Card
--  -> HTML a InternalGameAction
renderCardInHand player playerIndex cardIndex card =
  renderCard
  (const $ Just $ MakePlay $ PlayCard { playerIndex,  cardIndex })
  player
  card

handleAction
  :: forall s p m
  . Log m
  => Random m
  => InternalGameAction
  -> HalogenM ComponentState p s GameEvent m Unit
handleAction = case _ of
  WritePlayerIndex index -> do
    { nextPlayerIndex, nextPlayerCount } <- H.get
    log $ "Domination: updating playerIndex from "
      <> show nextPlayerIndex <> " -> " <> show index
    H.modify_ $ (_nextPlayerIndex .~ (max index 0))
      >>> (_nextPlayerCount .~ (max (index + 1) nextPlayerCount))
  WritePlayerCount count -> do
    { nextPlayerIndex, nextPlayerCount } <- H.get
    log $ "Domination: updating playerCount from "
      <> show nextPlayerCount <> " -> " <> show count
    H.modify_ $ (_nextPlayerCount .~ (max count 1))
      <<< (_nextPlayerIndex .~ (min (count - 1) nextPlayerIndex))
  StartNewGame -> do
    { nextPlayerIndex: playerIndex, nextPlayerCount: playerCount } <- H.get
    log $ "Domination: StartNewGame as player " <> show playerIndex
    playAndReport playerIndex $ NewGame { playerCount }
  MakePlay play -> do
    let playerIndex = fromMaybe 0 $ play ^? Play._playerIndex
    log "Domination: MakePlay"
    playAndReport playerIndex play
  LoadGameRequest -> H.raise LoadGame
  UndoRequest as -> H.raise $ Undo as

playAndReport
  :: forall s p m
  . Log m
  => Random m
  => Int
  -> Play
  -> HalogenM ComponentState p s GameEvent m Unit
playAndReport playerIndex play = do
  { maybeGame, nextPlayerCount, nextPlayerIndex } <- H.get
  case maybeGame of
    Nothing -> case play of
      NewGame { playerCount } -> doTheRest
      -- TODO: WritePlayerCount here
        { i: 0
        , state: Dom.newGame playerCount
        , playerCount: nextPlayerCount
        , playerIndex: nextPlayerIndex
        }
      _ -> error "Domination: playing with no game state?"
    Just activeState -> doTheRest activeState
  where
    doTheRest activeState = do
      result <- Dom.makeAutoPlay play activeState.state
      case result of
        Left e -> error e
        Right gs -> do
          let
            playMade = case play of
              EndPhase _ -> Nothing
              _ -> Just
                { play
                , playerIndex
                , state: activeState.state
                }
            newI = case play of
              NewGame _ -> 0
              _ -> activeState.i + 1
          H.modify_
            $ (_state .~ gs)
            >>> (_maybeGame <<< _Just <<< _playerIndex .~ activeState.playerIndex)
            >>> (_maybeGame <<< _Just <<< prop (SProxy :: SProxy "i") .~ newI)
          H.raise
            $ NewState (activeState { i = newI, state = gs }) playMade

