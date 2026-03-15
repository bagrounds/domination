module Domination.Data.AI where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Except.Trans (runExceptT)
import Data.Array (filter, findIndex, head, length, mapWithIndex, (..), (!!))
import Data.Array as Array
import Data.Array.NonEmpty (index) as NEA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst)
import Domination.Capability.Log (class Log, log)
import Domination.Capability.Random (class Random, randomElement, randomIntBetween)
import Domination.Data.AI.Strategy (Strategy(..), botName)
import Domination.Data.Card (Card)
import Domination.Data.Card as Card
import Domination.Data.Choice (Choice(..))
import Domination.Data.Game (Game)
import Domination.Data.Game as Game
import Domination.Data.Game.Engine as Engine
import Domination.Data.Phase (Phase(..))
import Domination.Data.Play (Play(..))
import Domination.Data.Player (Player)
import Domination.Data.Player as Player
import Domination.Data.Reaction (Reaction(..))

type Bot =
  { playerIndex :: Int
  , strategy :: Strategy
  }

findBotToAct :: Array Bot -> Game -> Maybe Bot
findBotToAct bots game =
  if game.result /= Nothing
  then Nothing
  else
    let actingPlayerIndex =
          if Game.choicesOutstanding game
          then Engine.choiceTurn game
          else game.turn
    in Array.find (_.playerIndex >>> (_ == actingPlayerIndex)) bots

generatePlay
  :: forall m
  . MonadError String m
  => Random m
  => Log m
  => Bot
  -> Game
  -> m Play
generatePlay { playerIndex, strategy } game = do
  log $ botName strategy <> " (Player " <> show (playerIndex + 1)
    <> ") is thinking..."
  case strategy of
    BigMoney -> bigMoneyPlay playerIndex game
    Random -> randomPlay playerIndex game

generateBotPlay
  :: forall m
  . Random m
  => Log m
  => Bot
  -> Game
  -> m (Either String Play)
generateBotPlay bot game = runExceptT $ generatePlay bot game

bigMoneyPlay
  :: forall m
  . MonadError String m
  => Random m
  => Log m
  => Int
  -> Game
  -> m Play
bigMoneyPlay playerIndex game =
  if Game.choicesOutstanding game
  then resolveOutstandingPlay playerIndex game
  else case game.phase of
    ActionPhase -> pure $ EndPhase { playerIndex }
    BuyPhase -> bigMoneyBuy playerIndex game
    CleanupPhase -> pure $ EndPhase { playerIndex }

bigMoneyBuy
  :: forall m
  . MonadError String m
  => Random m
  => Log m
  => Int
  -> Game
  -> m Play
bigMoneyBuy playerIndex game = do
  player <- Game.getPlayer playerIndex game
  let playerCash = Player.cash player
  log $ botName BigMoney <> " has $" <> show playerCash
  if player.buys <= zero
  then pure $ EndPhase { playerIndex }
  else
    case bigMoneyTarget playerCash game.supply of
      Nothing -> pure $ EndPhase { playerIndex }
      Just cardName ->
        let stacks = game.supply
        in case findIndex (_.card.name >>> (_ == cardName)) stacks of
          Nothing -> pure $ EndPhase { playerIndex }
          Just stackIndex -> do
            log $ botName BigMoney <> " buys " <> cardName
            pure $ Purchase { playerIndex, stackIndex }

bigMoneyTarget :: Int -> Array { card :: Card, count :: Int } -> Maybe String
bigMoneyTarget cash supply =
  let
    available name = Array.any
      (\s -> s.card.name == name && s.count > zero)
      supply
    provinceCount = fromMaybe zero
      $ _.count <$> Array.find (_.card.name >>> (_ == "Province")) supply
  in
    if cash >= 8 && available "Province"
    then Just "Province"
    else if cash >= 6 && available "Gold"
    then Just "Gold"
    else if cash >= 5 && provinceCount <= 5 && available "Duchy"
    then Just "Duchy"
    else if cash >= 3 && available "Silver"
    then Just "Silver"
    else Nothing

randomPlay
  :: forall m
  . MonadError String m
  => Random m
  => Log m
  => Int
  -> Game
  -> m Play
randomPlay playerIndex game =
  if Game.choicesOutstanding game
  then resolveOutstandingPlay playerIndex game
  else case game.phase of
    ActionPhase -> randomActionPlay playerIndex game
    BuyPhase -> randomBuyPlay playerIndex game
    CleanupPhase -> pure $ EndPhase { playerIndex }

randomActionPlay
  :: forall m
  . MonadError String m
  => Random m
  => Log m
  => Int
  -> Game
  -> m Play
randomActionPlay playerIndex game = do
  player <- Game.getPlayer playerIndex game
  let actionCards = mapWithIndex Tuple
        $ filter Card.isAction player.hand
  if Player.hasActions player && length actionCards > zero
  then do
    mbCard <- randomElement actionCards
    case mbCard of
      Nothing -> pure $ EndPhase { playerIndex }
      Just (Tuple _ card) ->
        case findIndex (_.name >>> (_ == card.name)) player.hand of
          Nothing -> pure $ EndPhase { playerIndex }
          Just cardIndex -> do
            log $ botName Random <> " plays " <> card.name
            pure $ PlayCard { playerIndex, cardIndex }
  else pure $ EndPhase { playerIndex }

randomBuyPlay
  :: forall m
  . MonadError String m
  => Random m
  => Log m
  => Int
  -> Game
  -> m Play
randomBuyPlay playerIndex game = do
  player <- Game.getPlayer playerIndex game
  let
    playerCash = Player.cash player
    affordable = mapWithIndex (\i s -> { i, s })
      $ filter (\s -> s.count > zero && s.card.cost <= playerCash)
      $ game.supply
  if player.buys > zero && length affordable > zero
  then do
    idx <- randomIntBetween 0 (length affordable - 1)
    case affordable !! idx of
      Nothing -> pure $ EndPhase { playerIndex }
      Just { i: stackIndex, s } -> do
        log $ botName Random <> " buys " <> s.card.name
        pure $ Purchase { playerIndex, stackIndex }
  else pure $ EndPhase { playerIndex }

resolveOutstandingPlay
  :: forall m
  . MonadError String m
  => Random m
  => Log m
  => Int
  -> Game
  -> m Play
resolveOutstandingPlay playerIndex game = do
  let choicePlayerIndex = Engine.choiceTurn game
  case NEA.index game.players choicePlayerIndex of
    Nothing -> pure $ EndPhase { playerIndex }
    Just player ->
      if Game.isAttacked choicePlayerIndex game
        && Player.hasReaction player
      then handleReaction choicePlayerIndex player
      else resolveFirstChoice choicePlayerIndex player

handleReaction
  :: forall m
  . MonadError String m
  => Random m
  => Log m
  => Int
  -> Player
  -> m Play
handleReaction playerIndex player =
  let reactions = player.pendingReactions
      hasBlock = Array.any (fst >>> (_ == BlockAttack)) reactions
  in
    if hasBlock
    then do
      log $ "Bot (Player " <> show (playerIndex + 1) <> ") blocks attack"
      pure $ React { playerIndex, reaction: Just BlockAttack }
    else pure $ DoneReacting { playerIndex }

resolveFirstChoice
  :: forall m
  . MonadError String m
  => Random m
  => Log m
  => Int
  -> Player
  -> m Play
resolveFirstChoice playerIndex player =
  case Player.firstChoice player of
    Nothing -> pure $ EndPhase { playerIndex }
    Just choice -> do
      log $ "Bot (Player " <> show (playerIndex + 1)
        <> ") resolves choice"
      pure $ ResolveChoice
        { playerIndex
        , choice: autoResolve choice
        }

autoResolve :: Choice -> Choice
autoResolve = case _ of
  If r -> If r { resolution = Just unit }
  And r -> And r { resolution = Just unit }
  Or r@{ choices } ->
    case head choices of
      Nothing -> Or r { resolution = Nothing }
      Just c -> Or r { resolution = Just c }
  PickN r@{ choices, n } ->
    PickN r { resolution = Just (Array.take n choices) }
  Option r -> Option r { resolution = Just true }
  MoveFromTo r -> MoveFromTo r { resolution = Just [ 0 ] }
  GainCard r -> GainCard r { resolution = Just "Copper" }
  GainCards r -> GainCards r { resolution = Just unit }
  GainActions r -> GainActions r { resolution = Just unit }
  GainBuys r -> GainBuys r { resolution = Just unit }
  Discard r -> Discard r { resolution = Just unit }
  Draw r -> Draw r { resolution = Just unit }
  GainBonus r -> GainBonus r { resolution = Just unit }
  c@(StackChoice _) -> c

assignBotIndices :: Int -> Array Strategy -> Array Bot
assignBotIndices humanIndex strategies =
  let
    totalPlayers = length strategies + 1
    nonHumanIndices = filter (_ /= humanIndex) $ 0 .. (totalPlayers - 1)
  in mapWithIndex
    (\i strategy -> { playerIndex: fromMaybe i (nonHumanIndices !! i)
                    , strategy
                    })
    strategies
