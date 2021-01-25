module Domination.UI.Domination where

import Prelude

import Data.Array (catMaybes, filter, head, length, nub, reverse, (!!), (:))
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Lens.Fold ((^?))
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

data GameAction
  = WritePlayerIndex Int
  | WritePlayerCount Int
  | LoadGame
  | StartNewGame
  | UpdateGameState GameEvent

data GameEvent
  = NewState GameState
  | PlayMade { play :: Play, playerIndex :: Int, state :: GameState }

data GameQuery a
  = UpdateState ComponentState a
  | MakeNewGame { playerCount :: Int, playerIndex :: Int } a

derive instance genericGameQuery :: Generic (GameQuery a) _

type ComponentState = { playerIndex :: Int, state :: GameState }

gameUi
  :: forall c m r a
  . Log m
  => Random m
  => { playerCount :: Int, playerIndex :: Int | r }
  -> (GameAction -> a)
  -> HTML (Component GameQuery c m a) a
gameUi { playerCount, playerIndex } wrap =
  HH.div_ $
  [ Util.incrementer
    { label: "Players: "
    , mbMin: (Just 1)
    , mbMax: Nothing
    , value: playerCount
    , setValue: WritePlayerCount >>> wrap
    }
  , Util.incrementer
    { label: "Player #: "
    , mbMin: (Just 1)
    , mbMax: Nothing
    , value: playerIndex + 1
    , setValue: (_ - 1) >>> WritePlayerIndex >>> wrap
    }
  , HH.div_
    [ HH.button
      [ HE.onClick \_ -> Just $ wrap $ StartNewGame ]
      [ HH.text $ "Start New " <> show playerCount
        <> " Player Game as Player " <> show (playerIndex + 1)
      ]
    , HH.button
      [ HE.onClick \_ -> Just $ wrap $ LoadGame ]
      [ HH.text "Load Game" ]
    ]
    , HH.slot
      (SProxy :: SProxy "Domination")
      0
      (component playerCount playerIndex)
      unit
      (Just <<< wrap <<< UpdateGameState)
    ]

_component = SProxy :: SProxy "Domination"

type Component o c m a =
  ComponentSlot HTML ("Domination" :: Slot o GameEvent Int | c) m a

component
  :: forall input m
  . Log m
  => Random m
  => Int
  -> Int
  -> H.Component HTML GameQuery input GameEvent m
component playerCount playerIndex =
  H.mkComponent { initialState, render, eval }
  where
  initialState _ = { playerIndex, state: Dom.newGame playerCount }
  render = renderPlayerN
  eval = H.mkEval H.defaultEval
    { initialize = Just $ NewGame { playerCount }
    , handleAction = handleAction
    , handleQuery = handleQuery
    }

handleQuery
  :: forall a m slots
  . Log m
  => Random m
  => GameQuery a
  -> H.HalogenM ComponentState Play slots GameEvent m (Maybe a)
handleQuery = case _ of
  MakeNewGame { playerCount, playerIndex } a -> do
    log "Domination: MakeNewGame"
    H.modify_ _ { playerIndex = playerIndex }
    playAndReport playerIndex (NewGame { playerCount })
    pure $ Just a
  UpdateState cs a -> do
    log "Domination: UpdateState"
    H.put cs
    pure $ Just a

type ChildComponents t1 t2 t3 =
  H.ComponentSlot HTML
  ( "MoveFromTo" :: H.Slot t1 Choice Int
  , "PickN" :: H.Slot t1 (Array Choice) Int
  | t2
  ) t3 Play

renderPlayerN :: forall t1 t2 t3.
  ComponentState ->
  HTML (ChildComponents t1 t2 t3) Play
renderPlayerN cs@{ playerIndex, state } = HH.div_
  [ h1__ "Domination"
  , HH.div_ $ renderPlayers cs
  ]

renderSupply'
  :: forall t1 t2 t3
  . ComponentState
  -> Player
  -> HTML (ChildComponents t1 t2 t3) Play
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
  -> Array (HTML a Play)
renderSupply player { playerIndex, state } =
  renderStackI `mapWithIndex` state.supply
  where
    renderStackI stackIndex =
      renderStack onClick player
      where
        onClick _ =
          Just $ Purchase { playerIndex,  stackIndex }

renderDeck :: forall a. Player -> HTML a Play
renderDeck player = HH.button
  [ HE.onClick (const Nothing), HP.class_ Css.deck ]
  [ HH.ul_
    [ HH.li [] [ HH.text "Discard" ]
    , HH.li [] [ HH.text $ show ((length player.discard) :: Int) ]
    ]
  ]

renderDiscard :: forall a. Player -> HTML a Play
renderDiscard player = HH.button
  [ HE.onClick (const Nothing), HP.class_ Css.discard ]
  [ HH.ul_
    [ HH.li [] [ HH.text "Deck" ]
    , HH.li [] [ HH.text $ show ((length player.deck) :: Int) ]
    ]
  ]

renderCard
  :: forall a
  . (MouseEvent -> Maybe Play)
  -> Player
  -> Card
  -> HTML a Play
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
  . (MouseEvent -> Maybe Play)
  -> Player
  -> Stack
  -> HTML a Play
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
  -> Array (HTML (ChildComponents t1 t2 t3) Play)
renderPlayers cs@{ playerIndex, state } =
  case state.players !! playerIndex of
    Nothing -> []
    Just player -> [ renderPlayer cs player ]

playerStats
  :: forall a
  . ComponentState
  -> Int
  -> Player
  -> (HTML a Play)
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
  :: forall t1 t2 t3
  . ComponentState
  -> Player
  -> HTML (ChildComponents t1 t2 t3) Play
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
          [ { clickEvent:
              React { playerIndex, reaction: Just BlockAttack }
            , text: "Yes"
            }
          , { clickEvent: React { playerIndex, reaction: Nothing }
            , text: "No"
            }
          ]
      renderChoice = map \choice -> case choice of
          If x ->
            acknowledge message clickEvent
            where
              message = "Checking " <> renderText choice
              clickEvent = playEvent If x unit
          And x@{ choices } ->
            acknowledge message clickEvent
            where
              message = renderText choice
              clickEvent = playEvent And x unit
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
              <<< ResolveChoice
              <<< ({ playerIndex, choice: _ })
              <<< PickN
              <<< (x { resolution = _ })
              <<< Just
            ]
          Option x ->
            chooseOne (renderText x.choice <> "?")
              [ { clickEvent: ResolveChoice
                  { playerIndex
                  , choice: Option x { resolution = Just true }
                  }
                , text: "Yes"
                }
              , { clickEvent: ResolveChoice
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
              <<< ResolveChoice
              <<< { playerIndex, choice: _ }
            ]
          GainCards x@{ n, cardName } ->
            acknowledge message clickEvent
            where
              message = renderText choice
              clickEvent = playEvent GainCards x unit
          GainActions x@{ n } ->
            acknowledge message clickEvent
            where
              message = renderText choice
              clickEvent = playEvent GainActions x unit
          GainBuys x@{ n } ->
            acknowledge message clickEvent
            where
              message = renderText choice
              clickEvent = playEvent GainBuys x unit
          Discard x@{ selection: SelectAll } ->
            acknowledge message clickEvent
            where
              message = renderText choice
              clickEvent = playEvent Discard x unit
          Draw x@{ n } -> acknowledge message clickEvent
            where
              message = renderText choice
              clickEvent = playEvent Draw x unit
          GainBonus x@{ bonus } -> acknowledge message clickEvent
            where
              message = renderText choice
              clickEvent = playEvent GainBonus x unit
        where
          playEvent
            :: forall r a
            . ({ resolution :: Maybe a | r } -> Choice)
            -> { resolution :: Maybe a | r }
            -> a
            -> Play
          playEvent mk x r = ResolveChoice
            { playerIndex
            , choice: mk x { resolution = Just r }
            }

renderNextPhaseButton
  :: forall w
  . { playerIndex :: Int, state :: GameState }
  -> HTML w Play
renderNextPhaseButton { playerIndex, state } =
  HH.button
    [ HP.class_ (Css.nextPhase)
    , HE.onClick $ const $ Just $ EndPhase { playerIndex }
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

renderStats :: forall w. ComponentState -> HTML w Play
renderStats cs = HH.ul
  [ HP.class_ Css.stats ]
  (playerStats cs `mapWithIndex` cs.state.players)

renderAtPlay :: forall w. Player -> HTML w Play
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

renderBuying :: forall w. Player -> HTML w Play
renderBuying currentPlayer =
  HH.ul [ HP.class_ Css.buying ] $ title : stacks
  where
    title = HH.li_ [ h3__ "Buying" ]
    stacks = renderStack (const Nothing) currentPlayer
      <$> (stackCards currentPlayer.buying)

renderHand :: forall a. Player -> ComponentState -> HTML a Play
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
  -> HTML a Play
renderCardInHand player playerIndex cardIndex card =
  renderCard
  (const $ Just $ PlayCard { playerIndex,  cardIndex })
  player
  card

handleAction
  :: forall s m
  . Log m
  => Random m
  => Play
  -> HalogenM ComponentState Play s GameEvent m Unit
handleAction play = do
  let playerIndex = fromMaybe 0 $ play ^? Play._playerIndex
  log "Domination: MakePlay"
  playAndReport playerIndex play

playAndReport
  :: forall s m
  . Log m
  => Random m
  => Int
  -> Play
  -> HalogenM ComponentState Play s GameEvent m Unit
playAndReport playerIndex play = do
  { state } <- H.get
  result <- Dom.makeAutoPlay play state
  case result of
    Left e -> error e
    Right gs -> do
      case play of
        EndPhase _ -> pure unit
        _ -> H.raise $ PlayMade
          { play
          , playerIndex
          , state
          }
      H.modify _ { state = gs }
        >>= _.state
        >>> NewState
        >>> H.raise

