module Domination.Data.Game.ResolveChoice where

import Prelude hiding (Ordering(..))
import Prim hiding (Constraint)

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Array (all, filter, head, intercalate, length, replicate, uncons, (!!), (:))
import Data.Array.NonEmpty as NonEmpty
import Data.Lens.Fold ((^?))
import Data.Lens.Index (ix)
import Data.Lens.Setter (over, (.~))
import Data.Lens.Traversal (traverseOf)
import Data.Maybe (Maybe(..))
import Domination.Capability.Log (class Log, log)
import Domination.Capability.Random (class Random)
import Domination.Data.Bonus (Bonus(..))
import Domination.Data.Card (Card, passFilter)
import Domination.Data.Choice (Choice(..))
import Domination.Data.Constraint (Constraint(..))
import Domination.Data.Constraint as Constraint
import Domination.Data.Filter (Filter)
import Domination.Data.Filter as Filter
import Domination.Data.Game (Game, getPlayer, modifyPlayer, modifyPlayerM, modifyStack)
import Domination.Data.Game as Game
import Domination.Data.Pile (Pile)
import Domination.Data.Pile as Pile
import Domination.Data.Player (describes)
import Domination.Data.Player as Player
import Domination.Data.SelectCards (SelectCards(..))
import Domination.Data.Stack as Stack
import Domination.Data.StackEvaluation (StackExpression(..), StackValue(..))
import Domination.Data.Supply (indexOfStack, nonEmptyStacks, stackByName)
import Domination.Data.Var (Var(..))
import Record (merge)
import Relation (Relation(..))
import Rule (check, lengthIs, (!<>), (!>), (<>!), (<@!))
import Util (dropIndices, fromJust, moveAll, takeIndices)

resolveChoice
  :: forall m
  . MonadError String m
  => Log m
  => Random m
  => { playerIndex :: Int, choice :: Choice }
  -> Game
  -> m Game
resolveChoice z@{ playerIndex, choice } state = do
  log $ "resolveChoice " <> show z
  case choice of
    StackChoice { attack, expression, stack, description } -> do
      { expr: expr', stack: stack', state: state' } <-
        evalStackChoice'
          playerIndex attack description expression stack state
      let
        update = case _ of
          StackChoice x -> pure $ StackChoice x
            { expression = expr'
            , stack = stack'
            }
          x -> throwError $ "Expected StackChoice, found: " <> show x

      case expr', stack' of
        [], [] ->
          -- This stack expression has been completely evaluated.
          -- Now we drop it to avoid an infinite loop
          pure state'
            >>= traverseOf (Game._player playerIndex) Player.dropChoice
        [], x ->
          throwError $ "leftover stack: " <> show x
        _, _ ->
          -- This stack expression is partially evaluated.
          -- We need user interaction to finish evaluation.
          -- We update the player's first choice to propagate the
          -- results of our partial evaluation.
          pure state'
            >>= traverseOf
              (Game._player playerIndex <<< Player._choices <<< ix 0)
              update

    MoveFromTo body ->
      moveFromTo playerIndex state body
        >>= traverseOf (Game._player playerIndex) Player.dropChoice

    GainCard { filter, destination, resolution: Just cardName } ->
      gainCard playerIndex state { filter, destination, cardName }
        >>= traverseOf (Game._player playerIndex) Player.dropChoice

    GainCards { n, cardName, destination, resolution: Just _ } ->
      gainCards playerIndex state { n, cardName, destination }
        >>= traverseOf (Game._player playerIndex) Player.dropChoice

    GainActions { n, resolution: Just _ } ->
      modifyPlayer playerIndex (Player.gainActions n) state
        >>= traverseOf (Game._player playerIndex) Player.dropChoice

    GainBuys { n, resolution: Just _ } ->
      modifyPlayer playerIndex (Player.gainBuys n) state
        >>= traverseOf (Game._player playerIndex) Player.dropChoice

    Discard { selection: SelectAll, resolution: Just _ } ->
      let update = moveAll Player._hand Player._toDiscard
      in modifyPlayer playerIndex update state
        >>= traverseOf (Game._player playerIndex) Player.dropChoice

    Draw { n, resolution: Just _ } ->
      modifyPlayerM playerIndex (Player.drawCards n) state
        >>= traverseOf (Game._player playerIndex) Player.dropChoice

    GainBonus { bonus, resolution: Just _ } ->
      modifyPlayer playerIndex (Player.gainBonus bonus) state
        >>= traverseOf (Game._player playerIndex) Player.dropChoice

    If
      { choice: choice'
      , otherwise
      , condition
      , resolution: Just _
      } -> do
      player <- getPlayer playerIndex state
      ok <- condition `describes` player
      modifyPlayer playerIndex (playerUpdate ok) state
        >>= traverseOf (Game._player playerIndex) Player.dropChoice
      where
        playerUpdate ok player =
          if ok
          then Player.gainChoice choice' player
          else case otherwise of
            Nothing -> player
            Just c -> Player.gainChoice c player

    And { choices, resolution: Just _ } ->
      modifyPlayer playerIndex (Player.gainChoices choices) state
        >>= traverseOf (Game._player playerIndex) Player.dropChoice

    Or { resolution: Just chosen } ->
      modifyPlayer playerIndex (Player.gainChoice chosen) state
        >>= traverseOf (Game._player playerIndex) Player.dropChoice

    PickN { n, resolution: Just choices } -> do
      check $ choices <@! lengthIs EQ n !<> "choices"
      modifyPlayer playerIndex (Player.gainChoices choices) state
        >>= traverseOf (Game._player playerIndex) Player.dropChoice

    Option { choice: choice', resolution: Just agree } ->
      let
        playerUpdate =
          if agree
          then Player.gainChoice choice'
          else identity
      in modifyPlayer playerIndex playerUpdate state
        >>= traverseOf (Game._player playerIndex) Player.dropChoice

    If { resolution: Nothing } -> unresolved
    And { resolution: Nothing } -> unresolved
    Or { resolution: Nothing } -> unresolved
    PickN { resolution: Nothing } -> unresolved
    Option { resolution: Nothing } -> unresolved
    GainCards { resolution: Nothing } -> unresolved
    GainCard { filter: cardFilter, resolution: Nothing } -> do
      let
        unfiltered :: Array Card
        unfiltered = _.card <$> nonEmptyStacks state.supply
        cards :: Array Card
        cards = (passFilter cardFilter) `filter` unfiltered
      case cards of
        [] -> pure state
        _ -> unresolved
    GainActions { resolution: Nothing } -> unresolved
    GainBuys { resolution: Nothing } -> unresolved
    Discard { resolution: Nothing } -> unresolved
    Draw { resolution: Nothing } -> unresolved
    GainBonus { resolution: Nothing } -> unresolved
  where
    unresolved =
      throwError $ "this is an unresolved choice: " <> show choice

evalStackChoice'
  :: forall m
  . MonadError String m
  => Log m
  => Random m
  => Int
  -> Boolean
  -> String
  -> Array StackExpression
  -> Array StackValue
  -> Game
  -> m
    { state :: Game
    , expr :: Array StackExpression
    , stack :: Array StackValue
    }
evalStackChoice' playerIndex attack description expression stack state' = do
  let
    evalStackChoice = evalStackChoice' playerIndex attack description
  log $ "evalStackChoice"
    <> "\n expression: " <> show expression
    <> "\n  stack: " <> show stack
    <> "\n  player 0 choices: "
    <>
      ( intercalate "\n    "
      $ show
      <$> (NonEmpty.head state'.players).choices
      )
  case uncons expression of
    Nothing -> pure { state: state', expr: [], stack }

    Just { head: term, tail: expressionTail } -> case term of
      StackGainBonusCash -> case uncons stack of
        Nothing -> throwError
          "StackGainBonusCash: empty stack"
        Just { head: value, tail: stackTail } -> case value of
          StackInt i ->
            let update = Player.gainBonus $ Cash i
            in modifyPlayer playerIndex update state'
              >>= evalStackChoice expressionTail stackTail
          x -> throwError $ "StackGainBonusCash:"
            <> " Expected StackInt, found: " <> show x

      StackIf { condition, following, otherwise } -> do
        { stack: result, state: state'' } <-
          evalStackChoice condition stack state'
        case uncons result of
          Nothing -> throwError $ "StackIf: condition"
            <> " evaluated to empty stack!"
          Just { head, tail } ->
            case head of
              StackBool b ->
                let
                  expr' =
                    if b
                    then following <> expressionTail
                    else otherwise <> expressionTail
                in evalStackChoice expr' tail state''
              x -> throwError $ "StackIf: condition must"
                <> " evaluate to a boolean, not: "
                <> show x

      StackPush value ->
        evalStackChoice expressionTail (value : stack) state'

      StackEquals value ->
        case uncons stack of
          Nothing -> throwError
            "StackEquals with empty Stack"
          Just { head: value', tail: stackTail } -> let
            stack' = StackBool (value' == value) : stackTail
            in evalStackChoice expressionTail stack' state'

      StackChooseCardFromSupply
        { cardName: Bound cardName
        , filter: Bound filter
        } -> do
          { count, card } <-
            stackByName cardName state'.supply
          check
            $ ("selected card" <>! _) >>> (card <@! _)
            $ passFilter filter !> "illegal card"
          check
            $ ("selected stack" <>! _) >>> (count <@! _)
            $ (_ > 0) !> "empty"
          let
            stack' = StackString cardName : stack
          evalStackChoice expressionTail stack' state'

      StackOption (Bound b) -> let value = StackBool b in
        evalStackChoice expressionTail (value : stack) state'

      StackOption Unbound ->
        pure { state: state', expr: expression, stack }

      StackChooseCardFromSupply
        { cardName: Unbound
        , filter: Bound _
        } -> pure { state: state', expr: expression, stack }

      StackChooseCardFromSupply
        { filter: Unbound
        } -> throwError $ "Unbound filter"

      StackChooseCards
        { cards: Bound cardIndices
        , filter: Bound filter
        , from: Bound source
        , n: Bound constraint
        } -> do
          let
            _source = Game._pile source playerIndex
            stack' = StackArrayInt cardIndices : stack
          sourcePile <- fromJust "failed to get source"
            $ state' ^? _source
          selected <- takeIndices cardIndices sourcePile
          remaining <- dropIndices cardIndices sourcePile
          Constraint.check
            constraint
            selected
            remaining
            sourcePile
          check
            $ ("selected cards" <>! _) >>> (selected <@! _)
            $ all (passFilter filter) !> "illegal choice in"
          evalStackChoice expressionTail stack' state'

      StackChooseCards
        { cards: Unbound
        , filter: Bound _
        , from: Bound _
        , n: Bound _
        } -> pure { state: state', expr: expression, stack }

      StackChooseCards
        { cards: Unbound
        } -> throwError $ "Unbound cards"

      StackChooseCards
        { filter: Unbound
        } -> throwError $ "Unbound filter"

      StackChooseCards
        { from: Unbound
        } -> throwError $ "Unbound from"

      StackChooseCards
        { n: Unbound
        } -> throwError $ "Unbound n"

      StackBind label -> do
        case uncons stack of
          Nothing -> throwError "StackBind with empty Stack"
          Just { head: stackValue, tail: stackTail } -> do
            case uncons expressionTail of
              Nothing -> throwError "nothing to bind to!"
              Just { head: nextExpr, tail: exprTail' } ->
                case stackValue, label, nextExpr of
                  StackFilter f
                  , "filter"
                  , StackChooseCardFromSupply x -> do
                    let
                      z = merge { filter: Bound f } x
                      top = StackChooseCardFromSupply z
                      expr' = top : exprTail'
                    evalStackChoice expr' stackTail state'
                  _, _, _ -> throwError
                    $ "unimplemented - "
                    <> "stackValue: " <> show stackValue
                    <> ", label: " <> show label
                    <> ", nextExpr: " <> show nextExpr

      StackDuplicate ->
        case head stack of
          Nothing -> throwError
            "StackDuplicate with empty Stack"
          Just v ->
            evalStackChoice expressionTail (v : stack) state'

      StackCostOf ->
        case uncons stack of
          Nothing -> throwError "StackCostOf with empty Stack"
          Just { head: StackInt cardIndex, tail: stackTail } -> do
            player <- getPlayer playerIndex state'
            { cost } <- Player.getCard cardIndex player
            let
              top = StackInt cost
              stack' = top : stackTail
            evalStackChoice expressionTail stack' state'
          Just { head } -> throwError $
            "can't get the cost of " <> show head

      StackGainTo destination ->
        case uncons stack of
          Nothing -> throwError "StackGainTo with empty Stack"
          Just { head: StackString cardName, tail: stackTail } ->
            gainCards
              playerIndex
              state'
              { n: one, cardName, destination }
            >>= evalStackChoice expressionTail stackTail
          Just { head } -> throwError $
            "can't gain " <> show head

      StackMoveCards { from, to } -> do
        log "StackMoveCards"
        case uncons stack of
          Nothing -> throwError "StackMoveCards with empty Stack"
          Just
            { head: StackArrayInt cardIndices, tail: stackTail } -> do
            state'' <- moveFromTo playerIndex state'
              { filter: Filter.Any
              , n: Unlimited
              , source: from
              , destination: to
              , resolution: Just cardIndices
              , attack
              }
            log $ "StackMoveCards from: " <> show from
              <> " to: " <> show to
              <> ". stack: " <> show stack
              <> " -> " <> show stackTail
              <> ". expr : " <> show expression
              <> " -> " <> show expressionTail
            evalStackChoice expressionTail stackTail state''
          Just { head } -> throwError $
            "can't discard " <> show head

      StackDiscard ->
        case uncons stack of
          Nothing -> throwError
            "StackDiscard with empty Stack"
          Just { head: StackArrayInt cardIndices, tail: stackTail } -> do
            state'' <- moveFromTo playerIndex state'
              { filter: Filter.Any
              , n: Unlimited
              , source: Pile.Hand
              , destination: Pile.ToDiscard
              , resolution: Just cardIndices
              , attack
              }
            evalStackChoice expressionTail stackTail state''
          Just { head } -> throwError $
            "can't discard " <> show head

      StackTrash ->
        case uncons stack of
          Nothing -> throwError "StackTrash with empty Stack"
          Just { head: StackArrayInt cardIndices, tail: stackTail } -> do
            state'' <- moveFromTo playerIndex state'
              { filter: Filter.Any
              , n: Unlimited
              , source: Pile.Hand
              , destination: Pile.Trash
              , resolution: Just cardIndices
              , attack
              }
            evalStackChoice expressionTail stackTail state''
          Just { head } -> throwError $
            "can't trash " <> show head

      StackLength ->
        case uncons stack of
          Nothing -> throwError "StackLength with empty Stack"
          Just { head: StackArrayInt ints, tail: stackTail } -> let
            stack' = StackInt (length ints) : stackTail
            in evalStackChoice expressionTail stack' state'
          Just { head } -> throwError $
            "can't take the length of " <> show head

      StackAddN n ->
        case uncons stack of
          Nothing -> throwError "StackAddN with empty Stack"
          Just { head: StackInt i, tail: stackTail } -> let
            stack' = StackInt (i + n) : stackTail
            in evalStackChoice expressionTail stack' state'
          Just { head } -> throwError $
            "can't add to " <> show head

      StackMakeFilterAnd ->
        case uncons stack of
          Nothing -> throwError
            "StackMakeFilterAnd with empty Stack"
          Just { head: StackFilter f1, tail: stackTail } ->
            case uncons stackTail of
              Nothing -> throwError
                "StackMakeFilterAnd requires 2 items on stack"
              Just { head: StackFilter f2, tail: stackTail' } -> do
                let
                  top = StackFilter $ Filter.And f1 f2
                  stack' = top : stackTail'
                evalStackChoice expressionTail stack' state'
              Just { head } -> throwError $
                "can't make a filter from " <> show head
                <> " stack: " <> show stack
                <> ", expression: " <> show expression
          Just { head } -> throwError $
            "can't make a filter from " <> show head
            <> " stack: " <> show stack
            <> ", expression: " <> show expression

      StackMakeFilterCostUpTo ->
        case uncons stack of
          Nothing -> throwError
            "StackMakeFilterCostUpTo with empty Stack"
          Just { head: StackInt n, tail: stackTail } -> do
            let
              top = StackFilter $ Filter.CostUpTo n
              stack' = top : stackTail
            evalStackChoice expressionTail stack' state'
          Just { head } -> throwError $
            "can't make a filter from " <> show head
            <> " stack: " <> show stack
            <> ", expression: " <> show expression

      StackNth n ->
        case uncons stack of
          Nothing -> throwError
            "StackNth with empty Stack"
          Just { head: StackArrayInt ints, tail: stackTail } -> do
            top :: Int <- fromJust
              ( "cannot get element " <> show n
              <> " from array " <> show ints
              )
              $ ints !! n
            let stack' = (StackInt top) : stackTail
            evalStackChoice expressionTail stack' state'
          Just { head } -> throwError $
            "can't take the nth of " <> show head

      StackDraw ->
        case uncons stack of
          Nothing -> throwError
            "StackDuplicate with empty Stack"
          Just { head: StackInt n, tail: stackTail } ->
            -- TODO: deduplicate code
            modifyPlayerM
              playerIndex
              (Player.drawCards n)
              state'
              >>= evalStackChoice expressionTail stackTail
          Just { head } -> throwError $
            "can't draw " <> show head

type MoveFromToRecord =
  { filter :: Filter
  , n :: Constraint
  , source :: Pile
  , destination :: Pile
  , resolution :: Maybe (Array Int)
  , attack :: Boolean
  }

moveFromTo
  :: forall m
  . MonadError String m
  => Random m
  => Int
  -> Game
  -> MoveFromToRecord
  -> m Game
moveFromTo _ _ choice@{ resolution: Nothing } =
  throwError $ "this is an unresolved choice: " <> show choice
moveFromTo playerIndex state
  { filter
  , n: constraint
  , source
  , destination
  , resolution: Just cardIndices
  } = do
  let
    _source = Game._pile source playerIndex
    _destination = Game._pile destination playerIndex
  sourcePile <- fromJust "failed to get source" $ state ^? _source
  selected <- takeIndices cardIndices sourcePile
  remaining <- dropIndices cardIndices sourcePile
  Constraint.check constraint selected remaining sourcePile
  check
    $ ("selected cards" <>! _) >>> (selected <@! _)
    $ all (passFilter filter) !> "illegal choice in"
  pure
    $ _source .~ remaining
    $ over _destination (selected <> _)
    $ state

gainCards
  :: forall m
  . MonadError String m
  => Random m
  => Int
  -> Game
  -> { n :: Int, cardName :: String, destination :: Pile }
  -> m Game
gainCards playerIndex state { n, cardName, destination } = do
  let _destination = Game._pile destination playerIndex
  { card, count } <- stackByName cardName state.supply
  stackIndex <- indexOfStack card state.supply
  let
    cardsToGain = min n count
    newCount = max zero (count - n)
    stackUpdate = Stack._count .~ newCount
    cards = replicate cardsToGain card
  over _destination (cards <> _)
    <$> modifyStack stackIndex stackUpdate state

gainCard
  :: forall m
  . MonadError String m
  => Random m
  => Int
  -> Game
  -> { filter :: Filter, cardName :: String, destination :: Pile }
  -> m Game
gainCard playerIndex state { destination, cardName } = do
  let _destination = Game._pile destination playerIndex
  stack <- stackByName cardName state.supply
  stackIndex <- indexOfStack stack.card state.supply
  let cards = [ stack.card ]
  over _destination (cards <> _)
    <$> modifyStack stackIndex Stack.take state


