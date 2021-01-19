module Domination.UI.Domination
  ( gameUi
  , component
  , Component
  , GameUpdate(..)
  , ComponentState(..)
  , GameEvent(..)
  ) where

import Prelude

import Data.Array (catMaybes, filter, head, length, nub, reverse, (!!), (:))
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Domination.Capability.Log (class Log, error, log)
import Domination.Capability.Random (class Random)
import Domination.Data.Card (Card)
import Domination.Data.Card (isAction, value) as Card
import Domination.Data.Choice (Choice(..))
import Domination.Data.GameState (GameState)
import Domination.Data.GameState as Dom
import Domination.Data.Phase (Phase(..))
import Domination.Data.Play (Play(..))
import Domination.Data.Player (Player)
import Domination.Data.Player as Player
import Domination.Data.Reaction (Reaction(..))
import Domination.Data.SelectCards (SelectCards(..))
import Domination.Data.Stack (Stack)
import Domination.UI.Bonus as Bonus
import Domination.UI.Card (render) as Card
import Domination.UI.Choice as Choice
import Domination.UI.ChoiceDiscardDownTo as Discard
import Domination.UI.ChoiceTrash as Trash
import Domination.UI.Css as Css
import Domination.UI.Phase as Phase
import Domination.UI.PickN as PickN
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

data GameEvent
  = NewState GameState
  | PlayMade Play

data GameUpdate
  = UpdateState ComponentState
  | MakeNewGame { playerCount :: Int, playerIndex :: Int }

derive instance genericGameUpdate :: Generic GameUpdate _
instance showGameUpdate :: Show GameUpdate where
  show = genericShow

data GameAction = Receive GameUpdate | MakePlay Play

derive instance genericGameAction :: Generic GameAction _
instance showGameAction :: Show GameAction where
  show = genericShow

type ComponentState = { playerIndex :: Int, state :: GameState }

gameUi newGame loadGame writeCount writeIndex state updateState =
  HH.div_ $
  [ Util.incrementer
    { label: "Players: "
    , mbMin: (Just 1)
    , mbMax: Nothing
    , value: state.playerCount
    , setValue: writeCount
    }
  , Util.incrementer
    { label: "Player #: "
    , mbMin: (Just 1)
    , mbMax: Nothing
    , value: state.playerIndex + 1
    , setValue: (_ - 1) >>> writeIndex
    }
  , HH.div_
    [ HH.button
      [ HE.onClick \_ -> Just $ newGame ]
      [ HH.text $ "Start New " <> show state.playerCount
        <> " Player Game as Player " <> show (state.playerIndex + 1)
      ]
    , HH.button
      [ HE.onClick \_ -> Just $ loadGame ]
      [ HH.text "Load Game" ]
    ]
  ] <> maybeGameUi
  where
    maybeGameUi = case state.gameState of
      Nothing -> []
      Just gameUpdate ->
        [ HH.slot
          (SProxy :: SProxy "Domination")
          0
          case gameUpdate of
            UpdateState { state: { players }, playerIndex } ->
              component (length players) playerIndex
            MakeNewGame { playerCount, playerIndex } ->
              component playerCount playerIndex
          gameUpdate
          (Just <<< updateState)
        ]

type Component o c m a =
  ComponentSlot HTML ("Domination" :: Slot o GameEvent Int | c) m a

component
  :: forall query m
  . Log m
  => Random m
  => Int -> Int -> H.Component HTML query GameUpdate GameEvent m
component playerCount playerIndex =
  H.mkComponent { initialState, render, eval }
  where
  initialState _ = { playerIndex, state: Dom.newGame playerCount }
  render = renderPlayerN
  eval = H.mkEval H.defaultEval
    { initialize = Just $ Receive $ MakeNewGame
      { playerCount, playerIndex }
    , handleAction = handleAction
    , receive = Just <<< Receive
    }

type ChildComponents t1 t2 t3 =
  H.ComponentSlot HTML
  ( "DiscardDownTo" :: H.Slot t1 Choice Int
  , "Trash" :: H.Slot t1 Choice Int
  , "PickN" :: H.Slot t1 (Array Choice) Int
  | t2
  ) t3 GameAction

renderPlayerN :: forall t1 t2 t3.
  ComponentState ->
  HTML (ChildComponents t1 t2 t3) GameAction
renderPlayerN cs@{ playerIndex, state } = HH.div_
  [ h1__ "Domination"
  , HH.div_ $ renderPlayers cs
  ]

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

renderSupply
  :: forall a
  . Player
  -> ComponentState
  -> Array (HTML a GameAction)
renderSupply player { playerIndex, state } =
  renderStackI `mapWithIndex` state.supply
  where
    renderStackI stackIndex =
      renderStack onClick player
      where
        onClick _ = Just $ MakePlay $ Purchase playerIndex stackIndex

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

renderCard
  :: forall a
  . (MouseEvent -> Maybe GameAction)
  -> Player
  -> Card
  -> HTML a GameAction
renderCard onClick player card =
  Card.render onClick extraClasses card
  where
    extraClasses = [
      if player.actions > 0 && Card.isAction card
      then Css.canPlay
      else Css.cantPlay
    ]

renderStack
  :: forall a
  . (MouseEvent -> Maybe GameAction)
  -> Player
  -> Stack
  -> HTML a GameAction
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
  :: forall t1 t2 t3
  . ComponentState
  -> Array (HTML (ChildComponents t1 t2 t3) GameAction)
renderPlayers cs@{ playerIndex, state } =
  case state.players !! playerIndex of
    Nothing -> []
    Just player -> [ renderPlayer cs player ]

playerStats
  :: forall a
  . ComponentState
  -> Int
  -> Player
  -> (HTML a GameAction)
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
      then " | " <> Phase.renderText state.phase
      else ""
    )
  ]

renderPlayer
  :: forall t1 t2 t3
  . ComponentState
  -> Player
  -> HTML (ChildComponents t1 t2 t3) GameAction
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
      renderReaction =
        Just $ chooseOne "Block attack?"
          [ { clickEvent: MakePlay
              $ React playerIndex (Just BlockAttack)
            , text: "Yes"
            }
          , { clickEvent: MakePlay $ React playerIndex Nothing
            , text: "No"
            }
          ]
      renderChoice = map \choice -> case choice of
          And x@{ choices } ->
            acknowledge message clickEvent
            where
              message = Choice.renderText' choice
              clickEvent = playEvent And x unit
          Or x@{ choices } ->
            chooseOne "Choose one" $
              choices <#> \choice' ->
                { clickEvent: playEvent Or x choice'
                , text: Choice.renderText' choice'
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
              (Just <<< MakePlay <<< ResolveChoice playerIndex <<< f)
            ]
            where
              f xs = PickN x { resolution = Just xs }
          Trash _ -> HH.div_
            [ HH.slot
              (SProxy :: SProxy "Trash")
              0
              (Trash.component player choice)
              unit
              (Just <<< MakePlay <<< ResolveChoice playerIndex)
            ]
          DiscardDownTo _ -> HH.div_
            [ HH.slot
              (SProxy :: SProxy "DiscardDownTo")
              2
              (Discard.component player choice)
              unit
              (Just <<< MakePlay <<< ResolveChoice playerIndex)
            ]
          GainCards x@{ n, cardName } ->
            acknowledge message clickEvent
            where
              message = Choice.renderText' choice
              clickEvent = playEvent GainCards x unit
          GainActions x@{ n } ->
            acknowledge message clickEvent
            where
              message = Choice.renderText' choice
              clickEvent = playEvent GainActions x unit
          GainBuys x@{ n } ->
            acknowledge message clickEvent
            where
              message = Choice.renderText' choice
              clickEvent = playEvent GainBuys x unit
          Discard x@{ selection: SelectAll } ->
            acknowledge message clickEvent
            where
              message = Choice.renderText' choice
              clickEvent = playEvent Discard x unit
          Draw x@{ n } -> acknowledge message clickEvent
            where
              message = Choice.renderText' choice
              clickEvent = playEvent Draw x unit
          GainBonus x@{ bonus } -> acknowledge message clickEvent
            where
              message = Choice.renderText' choice
              clickEvent = playEvent GainBonus x unit
        where
          playEvent
            :: forall r a
            . ({ resolution :: Maybe a | r } -> Choice)
            -> { resolution :: Maybe a | r }
            -> a
            -> GameAction
          playEvent mk x r = MakePlay $ ResolveChoice playerIndex
            $ mk x { resolution = Just r }

renderNextPhaseButton { playerIndex, state } =
  HH.button
    [ HP.class_ (Css.nextPhase)
    , HE.onClick \_ -> Just $ MakePlay $ EndPhase playerIndex
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
        <> " | " <> Phase.renderText state.phase
    ]

renderStats :: forall w. ComponentState -> HTML w GameAction
renderStats cs = HH.ul
  [ HP.class_ Css.stats ]
  (playerStats cs `mapWithIndex` cs.state.players)

renderAtPlay :: forall w. Player -> HTML w GameAction
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

renderBuying :: forall w. Player -> HTML w GameAction
renderBuying currentPlayer =
  HH.ul [ HP.class_ Css.buying ] $ title : stacks
  where
    title = HH.li_ [ h3__ "Buying" ]
    stacks = renderStack (const Nothing) currentPlayer
      <$> (stackCards currentPlayer.buying)

renderHand :: forall a. Player -> ComponentState -> HTML a GameAction
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

renderCardInHand
  :: forall a
  . Player
  -> Int
  -> Int
  -> Card
  -> HTML a GameAction
renderCardInHand player playerIndex cardIndex card =
  renderCard
  (\_ -> Just $ MakePlay $ PlayCard playerIndex cardIndex)
  player
  card

handleAction
  :: forall s m
  . Log m
  => Random m
  => GameAction
  -> HalogenM ComponentState GameAction s GameEvent m Unit
handleAction gameAction = do
  { state } <- H.get
  case gameAction of
    Receive update -> case update of
      MakeNewGame { playerCount, playerIndex } -> do
        log "Domination: MakeNewGame"
        H.modify_ _ { playerIndex = playerIndex }
        playAndReport (NewGame playerCount) state
      UpdateState cs -> do
        log "Domination: UpdateState"
        H.put cs
    MakePlay play -> do
        log "Domination: MakePlay"
        playAndReport play state
  where
    playAndReport play s = do
      result <- Dom.makeAutoPlay play s
      case result of
        Left e -> error e
        Right gs -> do
          case play of
            EndPhase _ ->
              pure unit
            _ ->
              H.raise $ PlayMade play
          H.modify _ { state = gs }
            >>= _.state
            >>> NewState
            >>> H.raise

