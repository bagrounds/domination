module Domination.Data.GameState
  ( GameState(..)
  , Supply
  , makeAutoPlay
  , makePlay
  , newGame
  , choiceTurn
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Except.Trans (runExceptT, throwError)
import Control.Monad.Loops (untilJust)
import Control.Monad.State (class MonadState, get, put)
import Data.Array (dropWhile, filter, findIndex, head, takeWhile, updateAt, (!!), (:))
import Data.Either (Either(..), note)
import Data.Foldable (any, foldM, length)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens.Fold (preview)
import Data.Lens.Getter (view)
import Data.Lens.Index (ix)
import Data.Lens.Lens (Lens')
import Data.Lens.Prism (Prism', prism')
import Data.Lens.Record (prop)
import Data.Lens.Traversal (Traversal', traverseOf)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Data.Traversable (sequence)
import Data.Tuple (fst, snd)
import Data.Unfoldable (replicate)
import Domination.Data.Card (Card, Command(..), Special)
import Domination.Data.Card as Card
import Domination.Data.CardType (CardType(..))
import Domination.Data.Choice (Choice(..))
import Domination.Data.Phase (Phase(..))
import Domination.Data.Phase as Phase
import Domination.Data.Play (Play(..))
import Domination.Data.Player (Player)
import Domination.Data.Player as Player
import Domination.Data.SelectCards (SelectCards(..))
import Domination.Data.Stack (Stack)
import Domination.Data.Target (Target(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Util (indices, justIf, withIndices)

type GameState =
  { turn :: Int
  , phase :: Phase
  , text :: String
  , players :: Array Player
  , supply :: Supply
  }

_turn :: Lens' GameState Int
_turn = prop (SProxy :: SProxy "turn")
_phase :: Lens' GameState Phase
_phase = prop (SProxy :: SProxy "phase")
_text :: Lens' GameState String
_text = prop (SProxy :: SProxy "text")
_players :: Lens' GameState (Array Player)
_players = prop (SProxy :: SProxy "players")
_supply :: Lens' GameState Supply
_supply = prop (SProxy :: SProxy "supply")

_player :: Int -> Traversal' GameState Player
_player i = _players <<< (ix i)
_stack :: Int -> Traversal' GameState Stack
_stack i = _supply <<< (ix i)

_ofPhase :: Phase -> Prism' GameState GameState
_ofPhase phase = prism' identity $ justIf ((==) phase <<< _.phase)

getPlayer :: Int -> GameState -> Maybe Player
getPlayer i = preview (_player i)

getStack :: Int -> GameState -> Maybe Stack
getStack i = preview (_stack i)

type Supply = Array Stack

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

makeAutoPlay :: forall m. MonadEffect m => MonadState GameState m => Play -> m (Either String Unit)
makeAutoPlay p = runExceptT $ makeAutoPlay' p

makeAutoPlay' :: forall m. MonadError String m => MonadEffect m => MonadState GameState m => Play -> m Unit
makeAutoPlay' = makePlay >=> const autoAdvance

makePlay :: forall m. MonadError String m => MonadEffect m => MonadState GameState m => Play -> m Unit
makePlay p = case p of
  NewGame n -> do
    eState <- setup (newGame n)
    case eState of
      Left e -> throwError e
      Right state -> do
        put state
        pure unit
  EndPhase playerIndex -> do
    gameState <- get
    maybeNewGameState <- nextPhase playerIndex gameState
    case maybeNewGameState of
      Left e -> throwError e
      Right state -> do
        put state
        pure unit
  PlayCard playerIndex card -> do
    gameState <- get
    maybeNewGameState <- play playerIndex card gameState
    case maybeNewGameState of
      Left e -> throwError e
      Right newGameState -> put newGameState <#> const unit
  Purchase playerIndex player stack -> do
    gameState <- get
    let maybeNewGameState = purchase playerIndex player stack gameState
    case note ("failed to make purchase") maybeNewGameState  of
      Left e -> throwError e
      Right newGameState -> put newGameState <#> const unit
  ResolveChoice playerIndex choice -> do
    state <- get
    let maybeNewGameState = resolveChoice playerIndex choice state
    case note ("failed to resolve choice") maybeNewGameState  of
      Left e -> throwError e
      Right newGameState -> put newGameState <#> const unit

currentPlayer :: GameState -> Maybe Player
currentPlayer state = state.players !! state.turn

autoAdvance :: forall m. MonadError String m => MonadEffect m => MonadState GameState m => m Unit
autoAdvance = untilJust autoAdvance'
  where
  autoAdvance' :: m (Maybe Unit)
  autoAdvance' = do
    gameState <- get
    liftEffect (Console.log $ "autoAdvance? from " <> show gameState.phase)
    case currentPlayer gameState of
      Nothing -> throwError "Something went wrong" -- TODO: return an error
      Just player -> case gameState.phase of
        ActionPhase -> if Player.hasActions player && Player.hasActionCardsInHand player
          then pure $ Just unit
          else advancePhase
        BuyPhase -> if player.buys > 0
          then pure $ Just unit
          else advancePhase
        CleanupPhase -> advancePhase
    where
      advancePhase :: m (Maybe Unit)
      advancePhase = do
        gameState <- get
        liftEffect $ Console.log $ "advancing from " <> show gameState.phase
        mbNewState <- nextPhase gameState.turn gameState
        case mbNewState of
          Left e -> throwError e
          Right newGameState -> const Nothing <$> put newGameState

nextPhase :: forall m. MonadEffect m => Int -> GameState -> m (Either String GameState)
nextPhase playerIndex state =
    if playerIndex /= state.turn
    then pure $ Left $ "error: not your turn! (nextPhase " <> show playerIndex <> " " <> show state <> ")"
    else if choicesOutstanding state
    then pure $ Left $ "error: choices need to be maide! (nextPhase" <> show playerIndex <> " " <> show state <> ")"
    else do
      let phase' = Phase.next state.phase
      let turn' = if state.phase == CleanupPhase
        then nextPlayer state
        else state.turn
      mbPlayers' :: (Array (Either String Player)) <- (if state.phase == CleanupPhase
          then sequence $
            mapWithIndex (\i p -> if i == playerIndex then Player.cleanup p else pure $ Right p)
            state.players
          else pure $ (Right <$> state.players))
      pure $ (sequence mbPlayers') <#> state { phase = phase', turn = turn', players = _ }

setup :: forall m. MonadEffect m => GameState -> m (Either String GameState)
setup gameState = do
  let (x :: Array (m (Either String Player))) = (Player.drawCards 5) <$> gameState.players
  x' :: Array (Either String Player) <- sequence x
  let (x'' :: Either String (Array Player)) = sequence x'
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

play :: forall m. MonadEffect m => Int -> Int -> GameState -> m (Either String GameState)
play playerIndex cardIndex originalState =
  if playerIndex /= originalState.turn
  then pure $ Left "error: play: not your turn!"
  else if choicesOutstanding originalState
  then pure $ Left "error: play: choices outstanding!"
  else case getPlayer playerIndex originalState of
  Nothing -> pure $ Left "error: play: bad error message"
  Just player' -> case Player.cardI cardIndex player' of
    Nothing -> pure $ Left "bad error message"
    Just card -> do
      player'' <- Player.play cardIndex player'
      case player'' of
        Left e -> pure $ Left e
        Right player''' -> case updateAt playerIndex player''' originalState.players of
          Nothing -> pure $ Left "bad error message"
          Just players' -> applySpecials originalState { players = players' } card
    where
    applySpecials :: GameState -> Card -> m (Either String GameState)
    applySpecials state card =
      foldM (\s c -> case s of
        Left e -> pure $ Left e
        Right s' -> applyEffectToTargets playerIndex s' c) (Right state) card.specials

    applyEffectToTargets :: Int -> GameState -> Special -> m (Either String GameState)
    applyEffectToTargets attackerIndex state { target, command } =
      foldM (\(state' :: Either String GameState) (i :: Int) ->
              case state' of
                   Left e -> pure $ Left e
                   Right s -> applyEffectToTarget command i s) (Right state) (targetIndices target attackerIndex state)

    applyEffectToTarget :: Command -> Int -> GameState -> m (Either String GameState)
    applyEffectToTarget (Gain card) targetIndex state = pure $ note "bad error message" $ do
      target <- getPlayer targetIndex state
      stackIndex <- findIndex (\x -> x.card == card) state.supply
      stack <- getStack stackIndex state
      let count' = if stack.count > 0 then stack.count - 1 else stack.count
      let stack' = stack { count = count' }
      supply' <- updateAt stackIndex stack' state.supply
      let target' = if stack.count > 0 then target { discard = stack.card : target.discard } else target
      players' <- updateAt targetIndex target' state.players
      pure (state { players = players', supply = supply' })
    applyEffectToTarget (Draw n) targetIndex state =
      case getPlayer targetIndex state of
      Nothing -> pure $ Left "bad error message"
      Just target -> do
        x :: Either String Player <- Player.drawCards n target
        let (x' :: Either String (Array Player) ) = x >>= \target' -> note "failed to update" $ updateAt targetIndex target' state.players
        pure $ x' <#> state { players = _ }
    applyEffectToTarget (Discard SelectAll) targetIndex state = pure $ (note "bad error message")
      $ getPlayer targetIndex state
      >>= \target ->
      let toDiscard' = target.toDiscard <> target.hand in
      let target' = target { toDiscard = toDiscard', hand = [] } in
      updateAt targetIndex target' state.players
      <#> state { players = _ }
    applyEffectToTarget (Choose choice) targetIndex state = pure $ (note "bad error messsage")
      $ maybeModifyPlayer targetIndex (Player.gainChoice choice >>> Just) state

    targetIndices :: Target -> Int -> GameState -> Array Int
    targetIndices EveryoneElse attackerIndex = filter (_ /= attackerIndex) <<< indices <<< _.players
    targetIndices Everyone _ = indices <<< _.players
    targetIndices Self attackerIndex = const [ attackerIndex ]

choicesOutstanding :: GameState -> Boolean
choicesOutstanding = any Player.hasChoices <<< view _players

maybeModifyPlayer :: Int -> (Player -> Maybe Player) -> GameState -> Maybe GameState
maybeModifyPlayer i = traverseOf (_player i)

maybeModifyPlayerHand :: Int -> (Array Card -> Maybe (Array Card)) -> GameState -> Maybe GameState
maybeModifyPlayerHand i = traverseOf (_player i <<< Player._hand)

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
copper = Card.treasure { name = "Copper", treasure = 1 }
silver :: Card
silver = Card.treasure { name = "Silver", cost = 3, treasure = 2 }
gold :: Card
gold = Card.treasure { name = "Gold", cost = 6, victoryPoints = 0, treasure = 3 }
platinum :: Card
platinum = Card.treasure { name = "Platinum", cost = 9, treasure = 5 }
estate :: Card
estate = Card.victory { name = "Estate", cost = 2, victoryPoints  = 1 }
duchy :: Card
duchy = Card.victory { name = "Duchy", cost = 5, victoryPoints = 3 }
province :: Card
province = Card.victory { name = "Province", cost = 8, victoryPoints = 6 }
colony :: Card
colony = Card.victory { name = "Colony", cost = 11, victoryPoints = 10 }
curse :: Card
curse = Card.card { types = [Curse], name = "Curse", victoryPoints = -1 }
greatHall :: Card
greatHall = Card.victory { types = [Action, Victory], name = "Great Hall", cost = 3, cards = 1, actions = 1, victoryPoints = 1 }
village :: Card
village = Card.action { name = "Village", cost = 3, cards = 1, actions = 2 }
woodCutter :: Card
woodCutter = Card.action { name = "Wood Cutter", cost = 3, buys = 1, treasure = 2 }
laboratory :: Card
laboratory = Card.action { name = "Laboratory", cost = 5, cards = 2, actions = 1 }
smithy :: Card
smithy = Card.action { name = "Smithy", cost = 4, cards = 3 }
festival :: Card
festival = Card.action { name = "Festival", cost = 5, actions = 2, buys = 1, treasure = 2 }
market :: Card
market = Card.action { name = "Market", cost = 5, actions = 1, cards = 1, buys = 1, treasure = 1 }
harem :: Card
harem = Card.treasure { types = [Treasure, Victory], name = "Harem", cost = 6, treasure = 2, victoryPoints = 2 }
bazaar :: Card
bazaar = Card.action { name = "Bazaar", cost = 5, cards = 1, actions = 2, treasure = 1 }
monument :: Card
monument = Card.action { types = [Action, Victory], name = "Monument", cost = 4, treasure = 2, victoryPoints = 1 }
workersVillage :: Card
workersVillage = Card.action { name = "Worker's Village", cost = 4, cards = 1, actions = 2, buys = 1 }
witch :: Card
witch = Card.actionAttack
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
councilRoom = Card.action
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
scholar = Card.action
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
chapel = Card.action
  { name = "Chapel"
  , cost = 2
  , specials =
    [ { target: Self
      , command: Choose $ (TrashUpTo 4 Nothing)
      , description: "Trash up to 4 cards from your hand"
      }
    ]
  }


