module Domination.UI.RenderText where

import Prelude

import Data.Array (intercalate)
import Data.Lens ((^?))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), fromMaybe)
import Domination.Data.Bonus (Bonus(..))
import Domination.Data.Choice (Choice(..))
import Domination.Data.Condition (Condition(..))
import Domination.Data.Constraint (Constraint(..))
import Domination.Data.GameState (GameState)
import Domination.Data.GameState as GameState
import Domination.Data.Phase (Phase(..))
import Domination.Data.Pile (Pile)
import Domination.Data.Pile as Pile
import Domination.Data.SelectCards (SelectCards(..))
import Domination.Data.WireInt (_WireInt)
import Util ((.^))

class RenderTextInContext a where
  renderTextInContext :: Int -> GameState -> a -> String

class RenderText a where
  renderText :: a -> String

instance phaseRenderText :: RenderText Phase where
  renderText ActionPhase = "Action Phase"
  renderText BuyPhase = "Buy Phase"
  renderText CleanupPhase = "Cleanup Phase"

instance bonusRenderText :: RenderText Bonus where
  renderText = case _ of
    Cash n -> "$" <> show (n .^ _WireInt)

instance conditionRenderText :: RenderText Condition where
  renderText = case _ of
    HasCard name -> "hand contains " <> name
    HasDiscard -> "discard pile is not empty"
    Randomly percent -> "randomly (" <> percentString <> "% chance)"
      where
        percentString = show $ percent .^ _WireInt

instance choiceRenderTextInContext :: RenderTextInContext Choice where
  renderTextInContext :: Int -> GameState -> Choice -> String
  renderTextInContext playerIndex state c =
    case c of
      If { choice, condition } ->
        "if " <> renderText condition <> " then " <> renderText choice
      And { choices } ->
        intercalate " and " (renderText <$> choices)
      Or { choices } ->
        "chose one of: "
        <> (intercalate ", " $ renderText <$> choices)
      PickN { n, choices, resolution } ->
        "chose " <> show n <> " of: "
        <> (intercalate ", " $ renderText <$> choices)
      Option { choice } ->
        "chose to optionally " <> renderText choice
      MoveFromTo { source, destination, resolution: (Just cardIndices) } ->
        case destination of
            Pile.Trash -> "trashed: " <> cards
            Pile.Discard -> "discarded: " <> cards
            Pile.Hand -> "gained: " <> cards <> " to their hand"
            Pile.Deck -> "gained: " <> cards <> " to their deck"
        where
          cards = intercalate ", "
            $ getCardName source playerIndex state <$> cardIndices
      GainCards { n, cardName } ->
        "gained " <> show n <> "x " <> cardName
      GainActions { n, resolution } ->
        "gained " <> show n <> " actions"
      GainBuys { n, resolution } ->
        "gained " <> show n <> " buys"
      GainBonus { bonus, resolution } ->
        "gained " <> renderText bonus
      Discard { selection: SelectAll } ->
        "discarded their hand"
      Draw { n, resolution } ->
        "drew " <> show n <> " cards"
      MoveFromTo { resolution: Nothing } ->
        unresolved
      where
        unresolved :: String
        unresolved = "unresolved choice? (" <> show c <> ")"

instance choiceRenderText :: RenderText Choice where
  renderText = case _ of
    If { condition, choice, otherwise } ->
      "If (" <> renderText condition <> ") then ("
      <> renderText choice <> ")"
      <> case otherwise of
        Nothing -> ""
        Just o -> " otherwise (" <> renderText o <> ")"
    And { choices } ->
      intercalate " and " (parenthesize <<< renderText <$> choices)
    Or { choices } ->
      intercalate " or " (parenthesize <<< renderText <$> choices)
    PickN { n, choices } ->
      show n <> " of: " <> intercalate " or " (renderText <$> choices)
    Option { choice } ->
      "optionally " <> renderText choice
    MoveFromTo { n, destination } -> case n of
      UpTo n ->
        verb <> " up to " <> show (n .^ _WireInt)
      Exactly n ->
        verb <> " " <> show (n .^ _WireInt)
      DownTo n ->
        verb <> " down to " <> show (n .^ _WireInt)
      where
        verb = case destination of
          Pile.Hand -> "Gain to your hand"
          Pile.Discard -> "Discard"
          Pile.Deck -> "Put onto your deck"
          Pile.Trash -> "Trash"
    GainCards { n, cardName } ->
      "Gain " <> cardName <> " x" <> show n
    GainActions { n } ->
      "+" <> show n <> " actions"
    GainBuys { n } ->
      "+" <> show n <> " buys"
    GainBonus { bonus } ->
      "+" <> renderText bonus
    Discard { selection: SelectAll } ->
      "Discard your hand"
    Draw { n } ->
      "Draw " <> show n <> " cards"

getCardName :: Pile -> Int -> GameState -> Int -> String
getCardName source playerIndex state cardIndex =
  fromMaybe "???" $ _.name <$> state
  ^? GameState._pile source playerIndex <<< ix cardIndex

parenthesize :: String -> String
parenthesize s = "(" <> s <> ")"
