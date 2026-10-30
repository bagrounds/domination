module Domination.Data.AI
  ( Bot
  , DryRun
  , actingPlayerIndex
  , assignBotIndices
  , bigMoneyTarget
  , botLabel
  , candidatePlays
  , cardSelections
  , choiceResolutions
  , combinations
  , findBotToAct
  , generateBotPlay
  , generatePlay
  , isProgressingPlay
  , selectionSizes
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Data.Array (filter, findIndex, length, mapWithIndex, sortBy, (..))
import Data.Array as Array
import Data.Array.NonEmpty (index) as NEA
import Data.Either (Either(..), either)
import Data.Lens.Fold ((^?))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Ord (abs, comparing)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Domination.Capability.Log (class Log, log)
import Domination.Capability.Random (class Random, randomBoolean, randomElement, randomIntBetween, shuffle)
import Domination.Data.AI.Strategy (Strategy(..), botName)
import Domination.Data.Card (Card)
import Domination.Data.Card as Card
import Domination.Data.Choice (Choice(..))
import Domination.Data.Constraint (Constraint(..)) as Constraint
import Domination.Data.Filter (Filter)
import Domination.Data.Game (Game)
import Domination.Data.Game as Game
import Domination.Data.Game.Engine as Engine
import Domination.Data.Phase (Phase(..))
import Domination.Data.Pile (Pile)
import Domination.Data.Play (Play(..))
import Domination.Data.Player (Player)
import Domination.Data.Player as Player
import Domination.Data.Reaction (Reaction(..))
import Domination.Data.Stack (_stacksToCardsIso)
import Domination.Data.StackEvaluation (StackExpression(..))
import Domination.Data.Var (Var(..))

type Bot =
  { playerIndex :: Int
  , strategy :: Strategy
  }

newtype DryRun :: (Type -> Type) -> Type -> Type
newtype DryRun m a = DryRun (m a)

runDryRun :: forall m a. DryRun m a -> m a
runDryRun (DryRun m) = m

derive newtype instance functorDryRun :: Functor m => Functor (DryRun m)
derive newtype instance applyDryRun :: Apply m => Apply (DryRun m)
derive newtype instance applicativeDryRun :: Applicative m => Applicative (DryRun m)
derive newtype instance bindDryRun :: Bind m => Bind (DryRun m)
derive newtype instance monadDryRun :: Monad m => Monad (DryRun m)

instance logDryRun :: Monad m => Log (DryRun m) where
  log = const $ pure unit
  error = const $ pure unit

instance randomDryRun :: Random m => Random (DryRun m) where
  shuffle = DryRun <<< shuffle
  randomElement = DryRun <<< randomElement
  randomIntBetween a = DryRun <<< randomIntBetween a
  randomBoolean = DryRun randomBoolean

actingPlayerIndex :: Game -> Int
actingPlayerIndex game =
  if Game.choicesOutstanding game
  then Engine.choiceTurn game
  else game.turn

findBotToAct :: Array Bot -> Game -> Maybe Bot
findBotToAct bots game = case game.result of
  Just _ -> Nothing
  Nothing -> Array.find (_.playerIndex >>> eq (actingPlayerIndex game)) bots

botLabel :: Bot -> String
botLabel { playerIndex, strategy } =
  botName strategy <> " (Player " <> show (playerIndex + 1) <> ")"

generatePlay
  :: forall m
  . MonadError String m
  => Random m
  => Log m
  => Bot
  -> Game
  -> m Play
generatePlay bot game = do
  candidates <- candidatePlays bot game
  chosen <- findM (isProgressingPlay game) candidates
  maybe (noLegalPlay candidates >>= throwError) (logChoice bot) chosen
  where
    noLegalPlay candidates = do
      verdicts <- traverse (playVerdict game) candidates
      pure $ botLabel bot
        <> " found no legal play among "
        <> show (length candidates)
        <> " candidates: "
        <> Array.intercalate "; " verdicts

playVerdict :: forall m. Random m => Game -> Play -> m String
playVerdict game play = describe <$> dryRun (Engine.makePlay play game)
  where
    describe = case _ of
      Left reason -> show play <> " was rejected: " <> reason
      Right game'
        | game' == game -> show play <> " made no progress"
        | otherwise -> show play <> " is legal"

logChoice :: forall m. Log m => Bot -> Play -> m Play
logChoice bot play = do
  log $ botLabel bot <> " chose " <> show play
  pure play

generateBotPlay
  :: forall m
  . Random m
  => Log m
  => Bot
  -> Game
  -> m (Either String Play)
generateBotPlay bot game = runExceptT $ generatePlay bot game

isProgressingPlay :: forall m. Random m => Game -> Play -> m Boolean
isProgressingPlay game play =
  either (const false) (_ /= game) <$> dryRun (Engine.makePlay play game)

dryRun
  :: forall m a
  . Random m
  => ExceptT String (DryRun m) a
  -> m (Either String a)
dryRun = runDryRun <<< runExceptT

findM :: forall m a. Monad m => (a -> m Boolean) -> Array a -> m (Maybe a)
findM predicate candidates = case Array.uncons candidates of
  Nothing -> pure Nothing
  Just { head, tail } -> predicate head >>= case _ of
    true -> pure $ Just head
    false -> findM predicate tail

candidatePlays :: forall m. Random m => Bot -> Game -> m (Array Play)
candidatePlays { playerIndex, strategy } game =
  maybe (pure []) plays $ NEA.index game.players playerIndex
  where
    plays player =
      if Game.choicesOutstanding game
      then choicePlays strategy playerIndex game player
      else turnPlays strategy playerIndex game player

turnPlays
  :: forall m
  . Random m
  => Strategy
  -> Int
  -> Game
  -> Player
  -> m (Array Play)
turnPlays strategy playerIndex game player = case game.phase of
  ActionPhase -> (_ <> endPhase) <$> actionPlays strategy playerIndex player
  BuyPhase -> (_ <> endPhase) <$> purchasePlays strategy playerIndex game player
  CleanupPhase -> pure endPhase
  where
    endPhase = [ EndPhase { playerIndex } ]

actionPlays
  :: forall m
  . Random m
  => Strategy
  -> Int
  -> Player
  -> m (Array Play)
actionPlays strategy playerIndex player = case strategy of
  BigMoney -> pure []
  Random -> shuffle $ playCard <$> actionCardIndices
  where
    playCard cardIndex = PlayCard { playerIndex, cardIndex }
    actionCardIndices = map fst
      $ filter (snd >>> Card.isAction)
      $ mapWithIndex Tuple player.hand

purchasePlays
  :: forall m
  . Random m
  => Strategy
  -> Int
  -> Game
  -> Player
  -> m (Array Play)
purchasePlays strategy playerIndex game player = case strategy of
  BigMoney -> pure $ purchase <$> Array.fromFoldable bigMoneyStackIndex
  Random -> shuffle $ purchase <$> nonEmptyStackIndices
  where
    purchase stackIndex = Purchase { playerIndex, stackIndex }
    bigMoneyStackIndex = do
      name <- bigMoneyTarget (Player.cash player) game.supply
      findIndex (_.card.name >>> eq name) game.supply
    nonEmptyStackIndices = map fst
      $ filter (snd >>> _.count >>> (_ > zero))
      $ mapWithIndex Tuple game.supply

bigMoneyTarget :: Int -> Array { card :: Card, count :: Int } -> Maybe String
bigMoneyTarget cash supply =
  _.card.name <$> (greening <|> Array.findMap affordableNamed priorities)
  where
    affordable = filter (\s -> s.count > zero && s.card.cost <= cash) supply
    affordableNamed name = Array.find (_.card.name >>> eq name) affordable
    provincesLeft = maybe zero _.count
      $ Array.find (_.card.name >>> eq "Province") supply
    priorities = map fst $ filter snd
      [ Tuple "Colony" true
      , Tuple "Platinum" true
      , Tuple "Province" true
      , Tuple "Gold" true
      , Tuple "Duchy" (provincesLeft <= 5)
      , Tuple "Silver" true
      , Tuple "Estate" (provincesLeft <= 2)
      ]
    greening =
      if provincesLeft > zero
      then Nothing
      else Array.last
        $ sortBy (comparing \s -> Tuple s.card.victoryPoints s.card.cost)
        $ filter (_.card.victoryPoints >>> (_ > zero)) affordable

choicePlays
  :: forall m
  . Random m
  => Strategy
  -> Int
  -> Game
  -> Player
  -> m (Array Play)
choicePlays strategy playerIndex game player =
  if Game.isAttacked playerIndex game && Player.hasReaction player
  then reactionPlays strategy playerIndex player
  else maybe (pure []) resolutions $ Player.firstChoice player
  where
    resolutions choice = map resolve
      <$> choiceResolutions strategy playerIndex game choice
    resolve choice = ResolveChoice { playerIndex, choice }

reactionPlays
  :: forall m
  . Random m
  => Strategy
  -> Int
  -> Player
  -> m (Array Play)
reactionPlays strategy playerIndex player =
  (_ <> [ DoneReacting { playerIndex } ]) <<< map react
    <$> preferredReactions
  where
    react reaction = React { playerIndex, reaction: Just reaction }
    reactions = Array.nubEq $ fst <$> player.pendingReactions
    preferredReactions = case strategy of
      BigMoney -> pure $ filter (eq BlockAttack) reactions
      Random -> shuffle reactions

choiceResolutions
  :: forall m
  . Random m
  => Strategy
  -> Int
  -> Game
  -> Choice
  -> m (Array Choice)
choiceResolutions strategy playerIndex game = case _ of
  If r -> pure [ If r { resolution = Just unit } ]
  And r -> pure [ And r { resolution = Just unit } ]
  Or r -> arrange strategy
    $ (\chosen -> Or r { resolution = Just chosen }) <$> r.choices
  PickN r -> arrange strategy
    $ (\chosen -> PickN r { resolution = Just chosen })
    <$> combinations r.n r.choices
  Option r -> arrange strategy
    $ (\agree -> Option r { resolution = Just agree }) <$> [ true, false ]
  MoveFromTo r ->
    map (\selection -> MoveFromTo r { resolution = Just selection })
      <$> cardSelections strategy r.n r.filter
        (pileCards r.source playerIndex game)
  GainCard r ->
    map (\name -> GainCard r { resolution = Just name })
      <$> gainableCardNames strategy r.filter game
  GainCards r -> pure [ GainCards r { resolution = Just unit } ]
  GainActions r -> pure [ GainActions r { resolution = Just unit } ]
  GainBuys r -> pure [ GainBuys r { resolution = Just unit } ]
  Discard r -> pure [ Discard r { resolution = Just unit } ]
  Draw r -> pure [ Draw r { resolution = Just unit } ]
  GainBonus r -> pure [ GainBonus r { resolution = Just unit } ]
  StackChoice r -> case Array.uncons r.expression of
    Nothing -> pure [ StackChoice r ]
    Just { head, tail } ->
      map (\bound -> StackChoice r { expression = Array.cons bound tail })
        <$> bindStackExpression strategy playerIndex game head

bindStackExpression
  :: forall m
  . Random m
  => Strategy
  -> Int
  -> Game
  -> StackExpression
  -> m (Array StackExpression)
bindStackExpression strategy playerIndex game = case _ of
  StackChooseCards e@
    { cards: Unbound
    , filter: Bound cardFilter
    , from: Bound source
    , n: Bound constraint
    } ->
    map (\selection -> StackChooseCards e { cards = Bound selection })
      <$> cardSelections strategy constraint cardFilter
        (pileCards source playerIndex game)
  StackChooseCardFromSupply e@{ cardName: Unbound, filter: Bound cardFilter } ->
    map (\name -> StackChooseCardFromSupply e { cardName = Bound name })
      <$> gainableCardNames strategy cardFilter game
  StackOption Unbound -> arrange strategy
    $ StackOption <<< Bound <$> [ true, false ]
  expression -> pure [ expression ]

arrange :: forall m a. Random m => Eq a => Strategy -> Array a -> m (Array a)
arrange = case _ of
  BigMoney -> pure
  Random -> shuffle

pileCards :: Pile -> Int -> Game -> Array Card
pileCards pile playerIndex game = fromMaybe []
  $ game ^? Game._pile pile playerIndex <<< _stacksToCardsIso

gainableCardNames
  :: forall m
  . Random m
  => Strategy
  -> Filter
  -> Game
  -> m (Array String)
gainableCardNames strategy cardFilter game = map _.name <$> ordered
  where
    gainable = filter (Card.passFilter cardFilter)
      $ _.card <$> filter (_.count >>> (_ > zero)) game.supply
    ordered = case strategy of
      BigMoney -> pure $ sortBy (comparing bigMoneyGainPriority) gainable
      Random -> shuffle gainable

bigMoneyGainPriority :: Card -> Tuple Boolean Int
bigMoneyGainPriority card = Tuple (not $ Card.isTreasure card) (negate card.cost)

cardSelections
  :: forall m
  . Random m
  => Strategy
  -> Constraint.Constraint
  -> Filter
  -> Array Card
  -> m (Array (Array Int))
cardSelections strategy constraint cardFilter cards =
  map (\size -> Array.sort $ Array.take size ordering) <$> sizes
  where
    eligible = filter (snd >>> Card.passFilter cardFilter)
      $ mapWithIndex Tuple cards
    allowedSizes = selectionSizes constraint
      { pileSize: length cards, eligible: length eligible }
    junkCount = length $ filter (snd >>> isJunk) eligible
    ordering = map fst $ sortBy (comparing (snd >>> keepPriority)) eligible
    sizes = case strategy of
      BigMoney -> pure $ sortBy (comparing (\size -> abs (size - junkCount))) allowedSizes
      Random -> shuffle allowedSizes

selectionSizes :: Constraint.Constraint -> { pileSize :: Int, eligible :: Int } -> Array Int
selectionSizes constraint { pileSize, eligible } = case constraint of
  Constraint.UpTo n -> upTo $ min n eligible
  Constraint.Exactly n -> [ min n pileSize ]
  Constraint.DownTo n -> [ max zero (pileSize - n) ]
  Constraint.Unlimited -> upTo eligible
  where
    upTo n = 0 .. max zero n

isJunk :: Card -> Boolean
isJunk card = not (Card.isAction card || Card.isTreasure card)

keepPriority :: Card -> Tuple Boolean Int
keepPriority card = Tuple (not $ isJunk card) (card.cost + card.treasure)

combinations :: forall a. Int -> Array a -> Array (Array a)
combinations k xs = case k <= zero, Array.uncons xs of
  true, _ -> [ [] ]
  false, Nothing -> []
  false, Just { head, tail } ->
    map (Array.cons head) (combinations (k - one) tail)
      <> combinations k tail

assignBotIndices :: Int -> Array Strategy -> Array Bot
assignBotIndices humanIndex strategies =
  let
    totalPlayers = length strategies + 1
    nonHumanIndices = filter (_ /= humanIndex) $ 0 .. (totalPlayers - 1)
  in Array.zipWith (\idx strategy -> { playerIndex: idx, strategy })
    nonHumanIndices
    strategies
