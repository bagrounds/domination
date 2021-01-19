module Domination.UI.Choice where

import Prelude

import Data.Array (intercalate)
import Data.Lens.Fold (preview)
import Data.Maybe (Maybe(..), fromMaybe)
import Domination.Data.Choice (Choice(..))
import Domination.Data.Constraint (Constraint(..))
import Domination.Data.GameState (GameState)
import Domination.Data.GameState as GameState
import Domination.Data.Pile as Pile
import Domination.Data.Player as Player
import Domination.Data.SelectCards (SelectCards(..))
import Domination.UI.Bonus as Bonus

unresolved :: String
unresolved = "unresolved choice?"

renderText :: Int -> GameState -> Choice -> String
renderText playerIndex state =
  case _ of
    And { choices, resolution: Just unit } ->
      intercalate " and " (renderText playerIndex state <$> choices)
    And _ -> unresolved
    Or { choices } ->
      "chose one of: "
      <> (intercalate ", " $ renderText' <$> choices)
    Or _ -> unresolved
    PickN { n, choices } ->
      "chose " <> show n <> " of: "
      <> (intercalate ", " $ renderText' <$> choices)
    PickN _ -> unresolved
    Option { choice } ->
      "chose to optionally " <> renderText' choice
    Option _ -> unresolved
    MoveFromHand { resolution: (Just cardIndices) } ->
      "trashed: "
      <> intercalate
      ", "
      (getPlayerCardName playerIndex state <$> cardIndices)
    MoveFromHand _ -> unresolved
    GainCards { n, cardName, resolution: Just unit } ->
      "gained " <> show n <> "x " <> cardName
    GainCards _ -> unresolved
    GainActions { n, resolution: Just unit } ->
      "gained " <> show n <> " actions"
    GainActions _ -> unresolved
    GainBuys { n, resolution: Just unit } ->
      "gained " <> show n <> " buys"
    GainBuys _ -> unresolved
    GainBonus { bonus, resolution: Just unit } ->
      "gained " <> Bonus.renderText bonus
    GainBonus _ -> unresolved
    Discard { selection: SelectAll, resolution: Just unit } ->
      "discarded their hand"
    Discard _ -> unresolved
    Draw { n, resolution: Just unit } ->
      "drew " <> show n <> " cards"
    Draw _ -> unresolved

renderText' :: Choice -> String
renderText' = case _ of
  And { choices } ->
    intercalate " and " (renderText' <$> choices)
  Or { choices } ->
    intercalate " or " (renderText' <$> choices)
  PickN { n, choices } ->
    show n <> " of: " <> intercalate " or " (renderText' <$> choices)
  Option { choice } ->
    "optionally " <> renderText' choice
  MoveFromHand { n, destination } -> case n of
    UpTo n ->
      verb <> " up to " <> show n
    Exactly n ->
      verb <> " " <> show n <> " (or as many as you have)"
    DownTo n ->
      verb <> " down to " <> show n
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
    "+" <> Bonus.renderText bonus
  Discard { selection: SelectAll } ->
    "Discard your hand"
  Draw { n } ->
    "Draw " <> show n <> " cards"

getPlayerCardName :: Int -> GameState -> Int -> String
getPlayerCardName playerIndex state cardIndex =
  fromMaybe "???"
  $ _.name
  <$> preview
  (GameState._player playerIndex <<< Player._cardInHand cardIndex)
  state

