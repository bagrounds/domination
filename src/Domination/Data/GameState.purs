module Domination.Data.GameState
  ( GameState(..)
  , Supply
  , Stack
  , Play(..)
  , makeAutoPlay
  , makePlay
  , newGame
  , choiceTurn
  ) where

import Prelude

import Control.Monad.Loops (untilJust)
import Control.Monad.State (class MonadState, get, modify_, put)
import Data.Array (dropWhile, filter, findIndex, head, takeWhile, updateAt, (!!), (:))
import Data.Either (Either(..), note)
import Data.Foldable (any, foldM, length)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens.Fold (preview)
import Data.Lens.Index (ix)
import Data.Lens.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Lens.Traversal (Traversal', traverseOf)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse, sequence)
import Data.Tuple (fst, snd)
import Data.Unfoldable (replicate)
import Domination.Data.Card (Card, Command(..), SelectCards(..), Special, Target(..), action, actionAttack, card, treasure, victory)
import Domination.Data.CardType (CardType(..))
import Domination.Data.Choice (Choice(..))
import Domination.Data.Phase (Phase(..))
import Domination.Data.Phase as Phase
import Domination.Data.Player (Player, _hand)
import Domination.Data.Player as Player
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Util (indices, withIndices)

data Play
  = NewGame Int
  | EndPhase Int
  | PlayCard Int Int
  | Purchase Int Player Stack
  | ResolveChoice Int Choice

derive instance genericPlay :: Generic Play _
instance showPlay :: Show Play where
  show = genericShow

makeAutoPlay :: forall m. MonadEffect m => MonadState GameState m => Play -> m (Either String Unit)
makeAutoPlay = makePlay >=> const autoAdvance

makePlay :: forall m. MonadEffect m => MonadState GameState m => Play -> m (Either String Unit)
makePlay = case _ of
  NewGame n -> do
    maybeState <- setup (newGame n)
    case note "Something went wrong" maybeState of
      Left e -> pure $ Left e
      Right state -> do
        put state
        pure $ Right unit
  EndPhase playerIndex -> do
    gameState <- get
    maybeNewGameState <- nextPhase playerIndex gameState
    case note "Something went wrong" maybeNewGameState of
      Left e -> pure $ Left e
      Right state -> do
        put state
        pure $ Right unit
  PlayCard playerIndex card -> do
    gameState <- get
    maybeNewGameState <- play playerIndex card gameState
    case note "Something went wrong" maybeNewGameState of
      Left e -> pure $ Left e
      Right newGameState -> put newGameState <#> const Right unit
  Purchase playerIndex player stack -> do
    gameState <- get
    let maybeNewGameState = purchase playerIndex player stack gameState
    case note "Something went wrong" maybeNewGameState  of
      Left e -> pure $ Left e
      Right newGameState -> put newGameState <#> const Right unit
  ResolveChoice playerIndex choice -> do
    state <- get
    let maybeNewGameState = resolveChoice playerIndex choice state
    case note "Something went wrong" maybeNewGameState  of
      Left e -> pure $ Left e
      Right newGameState -> put newGameState <#> const Right unit

currentPlayer :: GameState -> Maybe Player
currentPlayer state = state.players !! state.turn

autoAdvance :: forall m. MonadEffect m => MonadState GameState m => m (Either String Unit)
autoAdvance = untilJust autoAdvance'
  where
  autoAdvance' :: m (Maybe (Either String Unit))
  autoAdvance' = do
    gameState <- get
    liftEffect (Console.log $ "autoAdvance? from " <> show gameState.phase)
    case currentPlayer gameState of
      Nothing -> pure $ Just $ Left "Something went wrong" -- TODO: return an error
      Just player -> case gameState.phase of
        ActionPhase -> if Player.hasActions player && Player.hasActionCardsInHand player
          then pure $ Just $ Right unit
          else advancePhase
        BuyPhase -> if player.buys > 0
          then pure $ Just $ Right unit
          else advancePhase
        CleanupPhase -> advancePhase
    where
      advancePhase :: m (Maybe (Either String Unit))
      advancePhase = do
        gameState <- get
        liftEffect $ Console.log $ "advancing from " <> show gameState.phase
        mbNewState <- nextPhase gameState.turn gameState
        case mbNewState of
          Nothing -> pure (Just $ Right unit)
          Just newGameState -> const Nothing <$> put newGameState

type GameState =
  { turn :: Int
  , phase :: Phase
  , text :: String
  , players :: Array Player
  , supply :: Supply
  }

type Supply = Array Stack
type Stack = { card :: Card, count :: Int }

newGame :: Int -> GameState
newGame i =
  { turn: 0
  , phase: ActionPhase
  , text: ""
  , players: replicate i newPlayer
  , supply:
    [ { card: copper, count: 50 }
    , { card: silver, count: 50 }
    , { card: gold, count: 50 }
    , { card: platinum, count: 50 }
    , { card: estate, count: 8 }
    , { card: duchy, count: 8 }
    , { card: province, count: 8 }
    , { card: colony, count: 8 }
    , { card: curse, count: 10 * (i - 1) }
    , { card: chapel, count: 10 }
    , { card: greatHall, count: 8 }
    , { card: village, count: 10 }
    , { card: woodCutter, count: 10 }
    , { card: monument, count: 8 }
    , { card: smithy, count: 10 }
    , { card: workersVillage, count: 10 }
    , { card: bazaar, count: 10 }
    , { card: festival, count: 10 }
    , { card: laboratory, count: 10 }
    , { card: market, count: 10 }
    , { card: harem, count: 8 }
    , { card: witch, count: 10 }
    , { card: councilRoom, count: 10 }
    , { card: scholar, count: 10 }
    ]
  }

nextPhase :: forall m. MonadEffect m => Int -> GameState -> m (Maybe GameState)
nextPhase playerIndex state =
    if playerIndex /= state.turn
    || choicesOutstanding state
    then pure Nothing
    else do
      let phase' = Phase.next state.phase
      let turn' = if state.phase == CleanupPhase
        then nextPlayer state
        else state.turn
      mbPlayers' :: (Array (Maybe Player)) <- (if state.phase == CleanupPhase
          then sequence $
            mapWithIndex (\i p -> if i == playerIndex then Player.cleanup p else pure $ Just p)
            state.players
          else pure $ (Just <$> state.players))
      pure $ (sequence mbPlayers') <#> state { phase = phase', turn = turn', players = _ }

setup :: forall m. MonadEffect m => GameState -> m (Maybe GameState)
setup gameState = do
  let (x :: Array (m (Maybe Player))) = (Player.drawCards 5) <$> gameState.players
  x' :: Array (Maybe Player) <- sequence x
  let (x'' :: Maybe (Array Player)) = sequence x'
  pure $ gameState { players = _ } <$> x''

nextPlayer :: GameState -> Int
nextPlayer state = (state.turn + 1) `mod` (length state.players)

purchase :: Int -> Player -> Stack -> GameState -> Maybe GameState
purchase playerIndex player stack state =
  if playerIndex /= state.turn
  || state.phase /= BuyPhase
  || player.buys < 1
  || stack.count < 1
  || Player.cash player < stack.card.cost
  then Nothing
  else
  let player' = player { buying = stack.card : player.buying, buys = player.buys - 1 } in
  let stack' = stack { count = stack.count - 1 } in
  let supply' = (\s -> if s == stack then stack' else s) <$> state.supply in
  state { players = _, supply = supply' } <$> updateAt playerIndex player' state.players

resolveChoice :: Int -> Choice -> GameState -> Maybe GameState
resolveChoice playerIndex (TrashUpTo n Nothing) state = Nothing
resolveChoice playerIndex (TrashUpTo n (Just cardIndices)) state =
  if length cardIndices > n
  then Nothing
  else maybeModifyPlayer playerIndex ((Player.dropCards cardIndices) >=> Player.dropChoice) state

_players :: Lens' GameState (Array Player)
_players = prop (SProxy :: SProxy "players")

_player :: Int -> Traversal' GameState Player
_player i = _players <<< (ix i)

getPlayer :: Int -> GameState -> Maybe Player
getPlayer i = preview (_player i)

_supply :: Lens' GameState (Array Stack)
_supply = prop (SProxy :: SProxy "supply")

_stack :: Int -> Traversal' GameState Stack
_stack i = _supply <<< (ix i)

getStack :: Int -> GameState -> Maybe Stack
getStack i = preview (_stack i)

play :: forall m. MonadEffect m => Int -> Int -> GameState -> m (Maybe GameState)
play playerIndex cardIndex originalState =
  if playerIndex /= originalState.turn
  || choicesOutstanding originalState
  then pure Nothing
  else case getPlayer playerIndex originalState of
  Nothing -> pure Nothing
  Just player' -> case Player.cardI cardIndex player' of
    Nothing -> pure Nothing
    Just card -> do
      player'' <- Player.play cardIndex player'
      case player'' of
        Nothing -> pure Nothing
        Just player''' -> case updateAt playerIndex player''' originalState.players of
          Nothing -> pure Nothing
          Just players' -> Just <$> applySpecials originalState { players = players' } card
    where
    applySpecials :: GameState -> Card -> m GameState
    applySpecials state card =
      foldM (applyEffectToTargets playerIndex) state card.specials

    applyEffectToTargets :: Int -> GameState -> Special -> m GameState
    applyEffectToTargets attackerIndex state { target, command } =
      foldM (\state' i -> fromMaybe state' <$> applyEffectToTarget command state' i) state (targetIndices target attackerIndex state)

    applyEffectToTarget :: Command -> GameState -> Int -> m (Maybe GameState)
    applyEffectToTarget (Gain card) state targetIndex = pure do
      target <- getPlayer targetIndex state
      stackIndex <- findIndex (\x -> x.card == card) state.supply
      stack <- getStack stackIndex state
      let count' = if stack.count > 0 then stack.count - 1 else stack.count
      let stack' = stack { count = count' }
      supply' <- updateAt stackIndex stack' state.supply
      let target' = if stack.count > 0 then target { discard = stack.card : target.discard } else target
      players' <- updateAt targetIndex target' state.players
      pure (state { players = players', supply = supply' })
    applyEffectToTarget (Draw n) state targetIndex =
      case getPlayer targetIndex state of
      Nothing -> pure Nothing
      Just target -> do
        x :: Maybe Player <- Player.drawCards n target
        let (x' :: Maybe (Array Player) ) = x >>= \target' -> updateAt targetIndex target' state.players
        pure $ x' <#> state { players = _ }
    applyEffectToTarget (Discard SelectAll) state targetIndex = pure
      $ getPlayer targetIndex state
      >>= \target ->
      let toDiscard' = target.toDiscard <> target.hand in
      let target' = target { toDiscard = toDiscard', hand = [] } in
      updateAt targetIndex target' state.players
      <#> state { players = _ }
    applyEffectToTarget (Choose choice) state targetIndex = pure
      $ maybeModifyPlayer targetIndex (Player.gainChoice choice >>> Just) state

    targetIndices :: Target -> Int -> GameState -> Array Int
    targetIndices EveryoneElse attackerIndex = filter (_ /= attackerIndex) <<< indices <<< _.players
    targetIndices Everyone _ = indices <<< _.players
    targetIndices Self attackerIndex = const [ attackerIndex ]


choicesOutstanding :: GameState -> Boolean
choicesOutstanding state = Player.hasChoices `any` state.players

maybeModifyPlayer :: Int -> (Player -> Maybe Player) -> GameState -> Maybe GameState
maybeModifyPlayer i = traverseOf (_player i)

maybeModifyPlayerHand :: Int -> (Array Card -> Maybe (Array Card)) -> GameState -> Maybe GameState
maybeModifyPlayerHand i = traverseOf (_player i <<< _hand)

-- should we return a maybe here in case there are no players?
-- or should players be a non-empty list?
choiceTurn :: GameState -> Int
choiceTurn state =
  let players' = withIndices state.players in
  let prefix = takeWhile (fst >>> (_ /= state.turn)) players' in
  let suffix = dropWhile (fst >>> (_ /= state.turn)) players' in
  let rotated = suffix <> prefix in
  let withChoices = (snd >>> Player.hasChoices) `filter` rotated in
  fromMaybe 0 $ fst <$> (head withChoices)

newPlayer :: Player
newPlayer =
  { deck: []
  , hand: []
  , discard: (replicate 7 copper) <> (replicate 3 estate)
  , toDiscard: []
  , atPlay: []
  , buying: []
  , actions: 1
  , buys: 1
  , choices: []
  }

copper :: Card
copper = treasure { name = "Copper", treasure = 1 }
silver :: Card
silver = treasure { name = "Silver", cost = 3, treasure = 2 }
gold :: Card
gold = treasure { name = "Gold", cost = 6, victoryPoints = 0, treasure = 3 }
platinum :: Card
platinum = treasure { name = "Platinum", cost = 9, treasure = 5 }
estate :: Card
estate = victory { name = "Estate", cost = 2, victoryPoints  = 1 }
duchy :: Card
duchy = victory { name = "Duchy", cost = 5, victoryPoints = 3 }
province :: Card
province = victory { name = "Province", cost = 8, victoryPoints = 6 }
colony :: Card
colony = victory { name = "Colony", cost = 11, victoryPoints = 10 }
curse :: Card
curse = card { types = [Curse], name = "Curse", victoryPoints = -1 }
greatHall :: Card
greatHall = victory { types = [Action, Victory], name = "Great Hall", cost = 3, cards = 1, actions = 1, victoryPoints = 1 }
village :: Card
village = action { name = "Village", cost = 3, cards = 1, actions = 2 }
woodCutter :: Card
woodCutter = action { name = "Wood Cutter", cost = 3, buys = 1, treasure = 2 }
laboratory :: Card
laboratory = action { name = "Laboratory", cost = 5, cards = 2, actions = 1 }
smithy :: Card
smithy = action { name = "Smithy", cost = 4, cards = 3 }
festival :: Card
festival = action { name = "Festival", cost = 5, actions = 2, buys = 1, treasure = 2 }
market :: Card
market = action { name = "Market", cost = 5, actions = 1, cards = 1, buys = 1, treasure = 1 }
harem :: Card
harem = treasure { types = [Treasure, Victory], name = "Harem", cost = 6, treasure = 2, victoryPoints = 2 }
bazaar :: Card
bazaar = action { name = "Bazaar", cost = 5, cards = 1, actions = 2, treasure = 1 }
monument :: Card
monument = action { types = [Action, Victory], name = "Monument", cost = 4, treasure = 2, victoryPoints = 1 }
workersVillage :: Card
workersVillage = action { name = "Worker's Village", cost = 4, cards = 1, actions = 2, buys = 1 }
witch :: Card
witch = actionAttack
  { name = "Witch"
  , cost = 5
  , cards = 2
  , specials =
    [ { target: EveryoneElse
      , command: Gain curse
      , description: "Each other player gains a Curse."
      }
    ]
  }
councilRoom :: Card
councilRoom = action
  { name = "Council Room"
  , cost = 5
  , cards = 4
  , buys = 1
  , specials =
    [ { target: EveryoneElse
      , command: Draw 1
      , description: "Each other player draws a card."
      }
    ]
  }
scholar :: Card
scholar = action
  { name = "Scholar"
  , cost = 5
  , specials =
    [ { target: Self
      , command: Discard SelectAll
      , description: "Discard your hand."
      }
    , { target: Self
      , command: Draw 7
      , description: "Draw 7 cards"
      }
    ]
  }
chapel :: Card
chapel = action
  { name = "Chapel"
  , cost = 2
  , specials =
    [ { target: Self
      , command: Choose $ (TrashUpTo 4 Nothing)
      , description: "Trash up to 4 cards from your hand"
      }
    ]
  }


