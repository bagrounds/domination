module Domination.UI.Choice where

import Prelude

import Data.Array (intercalate)
import Data.Lens.Fold (preview)
import Data.Maybe (Maybe(..), fromMaybe)
import Domination.Data.Choice (Choice(..))
import Domination.Data.GameState (GameState)
import Domination.Data.GameState as GameState
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
    Or { choices, resolution: Just choice } ->
      "chose: " <> (intercalate " or " $ renderText' <$> choices)
    Or _ -> unresolved
    TrashUpTo { n, resolution: (Just cardIndices) } ->
      "trashed: "
      <> intercalate
      ", "
      (getPlayerCardName playerIndex state <$> cardIndices)
    TrashUpTo _ -> unresolved
    DiscardDownTo { n, resolution: (Just cardIndices) } ->
      "discarded: "
      <> intercalate
      ", "
      (getPlayerCardName playerIndex state <$> cardIndices)
    DiscardDownTo _ -> unresolved
    GainCards { n, cardName, resolution: Just unit } ->
      "gained " <> show n <> "x " <> cardName
    GainCards _ -> unresolved
    GainActions { n, resolution: Just unit } ->
      "gained " <> show n <> " actions"
    GainActions _ -> unresolved
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
  TrashUpTo { n } ->
    "Trash up to " <> show n
  DiscardDownTo { n } ->
    "Discard down to " <> show n
  GainCards { n, cardName } ->
    "Gain " <> show n <> "x " <> cardName
  GainActions { n } ->
    "Gain " <> show n <> " actions"
  GainBonus { bonus } ->
    "Gain " <> Bonus.renderText bonus
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

