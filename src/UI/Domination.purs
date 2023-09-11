module Domination.UI.Domination where

import Prelude

import AppAction (CardSpecSelection)
import AppState (Config)
import AppState as AppState
import Audio.WebAudio.Types (AudioContext)
import Data.Array (filter, findIndex, uncons, (:))
import Data.Array.NonEmpty ((!!))
import Data.Either (Either(..))
import Data.Foldable (length)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Lens.Fold ((^?))
import Data.Lens.Setter ((%~), (.~))
import Data.Maybe (Maybe(..), fromMaybe)
import Domination.Capability.Audio (class Audio, beep)
import Domination.Capability.Audio as Sound
import Domination.Capability.Dom (class Dom)
import Domination.Capability.Log (class Log, error, log)
import Domination.Capability.Random (class Random)
import Domination.Capability.Storage (class Storage)
import Domination.Data.Card (Card, _card, passFilter)
import Domination.Data.Card (isAction) as Card
import Domination.Data.Choice (Choice(..))
import Domination.Data.Game (Game)
import Domination.Data.Game (hasReaction, new, choicesOutstanding, isAttacked) as Game
import Domination.Data.Game.Engine (choiceTurn, makeAutoPlay) as Game
import Domination.Data.Phase (Phase(..))
import Domination.Data.Play (Play(..))
import Domination.Data.Play as Play
import Domination.Data.Player (Player)
import Domination.Data.Player as Player
import Domination.Data.Reaction (Reaction(..))
import Domination.Data.SelectCards (SelectCards(..))
import Domination.Data.Stack (Stack, stackCards)
import Domination.Data.StackEvaluation (StackExpression(..))
import Domination.Data.Supply (negativePoints, nonEmptyStacks, positivePoints)
import Domination.Data.Var (Var(..))
import Domination.UI.Card (render) as Card
import Domination.UI.ChoiceMoveFromTo as MoveFromTo
import Domination.UI.ChooseCards as ChooseCards
import Domination.UI.ChooseFromSupply as ChooseFromSupply
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
import Type.Proxy (Proxy(..))
import Web.UIEvent.MouseEvent (MouseEvent)

data GameQuery a
  = LoadActiveState ActiveState a
  | ReceiveGame { state :: Game, i :: Int } a
  | StartNewGameRequest AppState.Config a

derive instance genericGameQuery :: Generic (GameQuery a) _

_component :: Proxy "Domination"
_component = Proxy

type Component query r m action =
  ComponentSlot
  ( "Domination" :: Slot query GameEvent DomSlot
  , "description" :: H.Slot query Unit DomSlot
  | r
  )
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
  -> AudioContext
  -> H.Component GameQuery input GameEvent m
component config audioContext =
  H.mkComponent { initialState, render, eval }
  where
  initialState _ =
    { i: zero
    , playerIndex: config.nextPlayerIndex
    , playerCount: config.nextPlayerCount
    , state: Game.new one kingdom config.longGame
    , showSupply: true
    }
  kingdom = (_.cardSpec >>> _card) <$> _.selected `filter` config.kingdom
  render = renderPlayerN
  eval = H.mkEval H.defaultEval
    { handleAction = handleAction audioContext
    , handleQuery = handleQuery audioContext
    }

updateShowSupply :: ActiveState -> Game -> Boolean
updateShowSupply activeState { turn: nextTurn, phase } =
  case activeState, activeState.playerIndex, nextTurn, phase of
  { state: { turn } }, me, nt, BuyPhase
    | me == turn && me /= nt -> false
    | me == nt -> true
  _, me, nt, ActionPhase
    | me == nt -> false
  { showSupply }, _, _, _ -> showSupply


handleQuery
  :: forall action slots m a
  . Log m
  => Storage m
  => Audio m
  => Random m
  => AudioContext
  -> GameQuery a
  -> H.HalogenM ActiveState action slots GameEvent m (Maybe a)
handleQuery audioContext = case _ of
  ReceiveGame { state, i } a -> do
    activeGame <- H.get

    if Game.isAttacked activeGame.playerIndex state
    then beep audioContext Sound.Attacked
    else
      if activeGame.state.turn /= activeGame.playerIndex
      && state.turn == activeGame.playerIndex
      then beep audioContext Sound.YourTurn
      else pure unit

    let
      previousI = activeGame.i
      expectedI = previousI + one
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

  LoadActiveState activeState@{ state } a -> do
    activeGame <- H.get
    let
      newShowSupply = updateShowSupply activeGame state

    H.put $ (_showSupply .~ newShowSupply) activeState
    pure $ Just a

  StartNewGameRequest config a -> do
    let
      { kingdom
      , nextPlayerIndex: playerIndex
      , nextPlayerCount: playerCount
      , longGame
      } = config

    H.modify_ $ _playerIndex .~ playerIndex
    let (selecteds :: Array CardSpecSelection) = _.selected `filter` kingdom
    let (f :: CardSpecSelection -> Card) = (_.cardSpec >>> _card)
    let (supply :: Array Card) = f <$> selecteds
    playAndReport
      playerIndex
      (NewGame { playerCount, supply, longGame })
      audioContext
    pure $ Just a

type ChildComponents query r m =
  ComponentSlot
  ( "MoveFromTo" :: H.Slot query Choice DomSlot
  , "PickN" :: H.Slot query (Array Choice) DomSlot
  , "description" :: H.Slot query Unit DomSlot
  , "ChooseFromSupply" :: H.Slot query (Maybe String) DomSlot
  , "ChooseCards" :: H.Slot query (Array Int) DomSlot
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
    Just result ->
      [ Hud.render activeState
      , renderText result
      ]

renderSupply'
  :: forall query r m
  . Dom m
  => Log m
  => ActiveState
  -> Player
  -> HTML (ChildComponents query r m) Action
renderSupply' cs@{ showSupply, state, playerIndex } player =
  let
    activity =
      if state.turn == playerIndex
      && state.phase == BuyPhase
      then Css.active
      else Css.inactive
  in
  if not showSupply
  then HH.div
    [ HP.classes $ [ Css.supply, Css.collapsed, activity ] ]
    []
  else HH.div
    [ HP.classes $
      [ Css.supply
      , Css.showing
      , activity
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
    renderStackI stackIndex =
      renderStack onClick player (CardSlot SupplyArea stackIndex)
      where
        onClick _ =
          MakePlay $ Purchase { playerIndex, stackIndex }

renderDeck :: forall widget. Player -> HTML widget Action
renderDeck player = HH.button
  [ HE.onClick (const DoNothing), HP.class_ Css.deck ]
  [ HH.ul_
    [ HH.li [] [ HH.text "Discard" ]
    , HH.li [] [ HH.text $ show $ (length player.discard :: Int)]
    ]
  ]

renderDiscard :: forall widget. Player -> HTML widget Action
renderDiscard player = HH.button
  [ HE.onClick (const DoNothing), HP.class_ Css.discard ]
  [ HH.ul_
    [ HH.li [] [ HH.text "Deck" ]
    , HH.li [] [ HH.text $ show $ (length player.deck :: Int) ]
    ]
  ]

renderCard
  :: forall query r m
  . Dom m
  => Log m
  => (MouseEvent -> Action)
  -> Player
  -> Card
  -> DomSlot
  -> HTML (ChildComponents query r m) Action
renderCard onClick player card slotNumber =
  Card.render onClick extraClasses card slotNumber
  where
    extraClasses =
      [ if player.actions > zero
        && Card.isAction card
        then Css.canPlay
        else Css.cantPlay
      ]

renderStack
  :: forall query r m
  . Dom m
  => Log m
  => (MouseEvent -> Action)
  -> Player
  -> DomSlot
  -> Stack
  -> HTML (ChildComponents query r m) Action
renderStack onClick player slotNumber stack =
  HH.li
    [ HP.class_ Css.stack ]
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
        [ renderCard onClick player stack.card slotNumber ]
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
        <> " in a " <> show (length state.players :: Int)
        <> " player game."
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
    && playerIndex == Game.choiceTurn state
    then HH.div [ HP.class_ Css.dialogue ] $ fromMaybe [] $
      let
        choice = Player.firstChoice player
        hasReaction = Game.hasReaction playerIndex state
        isAttacked = Game.isAttacked playerIndex state
      in
      if isAttacked && hasReaction
      then pure <$> renderReaction
      else pure <$> renderChoice (CardSlot ChoiceArea) choice
    else HH.div_ []
  , case state.players !! state.turn of
      Nothing -> h1__ $ "No player (" <> show state.turn <> ") in "
        <> show state.players
      Just currentPlayer -> HH.div
        ( if state.turn /= playerIndex
          || Game.choicesOutstanding state
          then [ HP.class_ Css.waiting ]
          else []
        )
        [ renderSupply' cs player
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
            , HE.onClick \_ -> UndoRequest cs
            ]
            [ HH.text "Undo" ]
          , HH.button
            [ HP.class_ Css.supplyToggle
            , HE.onClick \_ -> ToggleSupply
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
          StackChoice x@{ expression } ->
            case uncons expression of
              Nothing ->
                h1__ "Something has gone terribly wrong!!!"

              Just
                { head: StackChooseCards
                  y@{ cards: Unbound
                  , filter: Bound filter
                  , from: Bound pile
                  , n: Bound constraint
                  }
                , tail
                } -> HH.div_
                [ HH.slot
                  (Proxy :: Proxy "ChooseCards")
                  (AreaSlot ChoiceArea)
                  ( ChooseCards.component
                    { state
                    , player
                    , baseSlotNumber
                    , pile
                    , constraint
                    , filter
                    }
                  )
                  unit
                  $ MakePlay
                  <<< ResolveChoice
                  <<< ({ playerIndex, choice: _ })
                  <<< StackChoice
                  <<< (x { expression = _ })
                  <<< (_ : tail)
                  <<< StackChooseCards
                  <<< (y { cards = _ })
                  <<< Bound
                ]

              Just
                { head: StackChooseCards { cards: Bound cards } } ->
                  h1__ $ "Domination: StackChooseCards:"
                    <> " cards already chosen: " <> show cards

              Just
                { head: StackChooseCardFromSupply
                  { cardName: Bound cardName }
                } -> h1__ $ "Domination: StackChooseCardFromSupply:"
                  <> " card already chosen: " <> show cardName

              Just
                { head: StackChooseCardFromSupply
                  y@{ cardName: Unbound
                  , filter: Bound cardFilter
                  }
                , tail
                } ->
                  let
                    predicate :: Card -> Boolean
                    predicate = passFilter cardFilter
                    unfiltered :: Array Card
                    unfiltered = _.card
                      <$> nonEmptyStacks state.supply
                    cards :: Array Card
                    cards = predicate `filter` unfiltered
                  in HH.div_
                    [ HH.slot
                      (Proxy :: Proxy "ChooseFromSupply")
                      (AreaSlot ChoiceArea)
                      ( ChooseFromSupply.component
                        { cards, baseSlotNumber }
                      )
                      unit
                      $ MakePlay
                      <<< ResolveChoice
                      <<< ({ playerIndex, choice: _ })
                      <<< StackChoice
                      <<< (x { expression = _ })
                      <<< (_ : tail)
                      <<< StackChooseCardFromSupply
                      <<< (y { cardName = _ })
                      <<< Bound
                      <<< fromMaybe "couldn't find card in supply"
                    ]

              Just
                { head: StackOption (Bound b)
                } -> h1__ $ "Domination: StackOption:"
                  <> " decision already made: " <> show b

              Just { head: StackOption Unbound, tail } ->
                chooseOne (HH.text x.description)
                  [ { clickEvent: MakePlay $ ResolveChoice
                      { playerIndex
                      , choice: StackChoice x
                        { expression = (StackOption $ Bound $ true)
                          : tail
                        }
                      }
                    , text: HH.text "Yes"
                    }
                  , { clickEvent: MakePlay $ ResolveChoice
                      { playerIndex
                      , choice: StackChoice x
                        { expression = (StackOption $ Bound $ false)
                          : tail
                        }
                      }
                    , text: HH.text "No"
                    }
                  ]

              Just _ ->
                acknowledge
                  (renderText choice)
                  ( MakePlay $ ResolveChoice
                    { playerIndex
                    , choice: StackChoice x
                    }
                  )

          If x ->
            acknowledge (renderText choice) (playEvent If x unit)

          And x ->
            acknowledge (renderText choice) (playEvent And x unit)

          Or x@{ choices } -> chooseOne (HH.text "Choose one")
            $ choices <#> \choice' ->
              { clickEvent: playEvent Or x choice'
              , text: renderText choice'
              }

          PickN x@{ n, choices } -> HH.div_
            [ HH.slot
              (Proxy :: Proxy "PickN")
              (AreaSlot ChoiceArea)
              ( PickN.component
                { title: "Choose " <> show n, n, choices }
              )
              unit
              $ MakePlay
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
              (Proxy :: Proxy "MoveFromTo")
              (AreaSlot ChoiceArea)
              ( MoveFromTo.component
                state
                player
                choice
                baseSlotNumber
              )
              unit
              $ MakePlay
              <<< ResolveChoice
              <<< { playerIndex, choice: _ }
            ]

          GainCards x -> acknowledge
            (renderText choice)
            (playEvent GainCards x unit)

          GainCard x@{ filter: cardFilter } ->
            let
              predicate :: Card -> Boolean
              predicate = passFilter cardFilter
              unfiltered :: Array Card
              unfiltered = _.card <$> nonEmptyStacks state.supply
              cards :: Array Card
              cards = predicate `filter` unfiltered
            in HH.div_
              [ HH.slot
                (Proxy :: Proxy "ChooseFromSupply")
                (AreaSlot ChoiceArea)
                (ChooseFromSupply.component { cards, baseSlotNumber })
                unit
                $ MakePlay
                <<< ResolveChoice
                <<< { playerIndex, choice: _ }
                <<< GainCard
                <<< (x { resolution = _ })
              ]

          GainActions x -> acknowledge
            (renderText choice)
            (playEvent GainActions x unit)

          GainBuys x -> acknowledge
            (renderText choice)
            (playEvent GainBuys x unit)

          Discard x@{ selection: SelectAll } ->
            acknowledge (renderText choice) (playEvent Discard x unit)

          Draw x ->
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
            { playerIndex, choice: mk x { resolution = Just r } }

renderNextPhaseButton
  :: forall w
  . ActiveState
  -> HTML w Action
renderNextPhaseButton { playerIndex, state } =
  HH.button
    [ HP.class_ Css.nextPhase
    , HE.onClick $ const $ MakePlay $ EndPhase { playerIndex }
    ]
    [ HH.span_ if state.turn == playerIndex
      then if Game.choicesOutstanding state
        then [ HH.text $ "Waiting for Player "
          <> show (Game.choiceTurn state + one)
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
    stacks =
      ( renderStack (const DoNothing) currentPlayer
      <<< (CardSlot AtPlayArea)
      ) `mapWithIndex` stackCards currentPlayer.atPlay

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
    stacks =
      ( renderStack (const DoNothing) currentPlayer
      <<< (CardSlot BuyingArea)
      ) `mapWithIndex` stackCards currentPlayer.buying

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
    || (length $ Card.isAction `filter` player.hand) < 1
    then [ Css.waiting ]
    else []
  ] $ title :
    ( \i s ->
      renderStack
        (onClick s player.hand)
        player
        (CardSlot HandArea i)
        s
    ) `mapWithIndex` (stackCards player.hand)
  where
    title = HH.li
      [ HP.class_ Css.handTitle ]
      [ HH.text "Hand" ]
    onClick stack hand = const $ MakePlay
      $ PlayCard { playerIndex,  cardIndex }
      where
        cardIndex = fromMaybe (-one)
          $ findIndex ((_ == stack.card.name) <<< _.name) hand

handleAction
  :: forall s p m
  . Log m
  => Random m
  => Audio m
  => AudioContext
  -> Action
  -> HalogenM ActiveState p s GameEvent m Unit
handleAction audioContext = case _ of
  MakePlay play ->
    let playerIndex = fromMaybe zero $ play ^? Play._playerIndex
    in playAndReport playerIndex play audioContext
  UndoRequest as -> H.raise $ Undo as
  ToggleSupply -> H.modify_ $ _showSupply %~ not
  DoNothing -> pure unit

playAndReport
  :: forall s p m
  . Log m
  => Random m
  => Audio m
  => Int
  -> Play
  -> AudioContext
  -> HalogenM ActiveState p s GameEvent m Unit
playAndReport playerIndex play audioContext = do
  activeState@{ state, showSupply } <- H.get
  let
    lastPhase = state.phase
    lastTurn = state.turn
  result <- Game.makeAutoPlay play activeState.state
  case result of
    Left e -> do
      beep audioContext Sound.Error
      error e
    Right gameState -> do
      case play of
        Purchase _ -> beep audioContext Sound.Purchase
        _ -> beep audioContext Sound.Acknowledge
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
      H.modify_ $ (_playerIndex .~ activeState.playerIndex)
        >>> (_i .~ newI)
        >>> (_showSupply .~ newShowSupply)
      H.raise $ NewState
        (((_i .~ newI) >>> (_state .~ gameState)) activeState)
        playMade

