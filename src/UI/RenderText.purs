--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| This code appears to be written in a functional programming language, likely Haskell, given the use of monads and higher-order functions.
--|
--| The provided code is a part of a game-like system, possibly a card game. It defines various data structures and functions for representing cards, piles, filters, and game state.
--|
--| Here's a brief overview of what the code does:
--|
--| 1. **Card representation**: The `getCardName` function takes a source pile, player index, game state, and card index as input. It returns the name of the card at that index in the specified pile.
--|
--| 2. **Pile representation**: The `_stacksToCardsIso` function is used to convert a pile into an array of cards.
--|
--| 3. **Game state representation**: The `Game._pile` accessors are used to retrieve piles from the game state.
--|
--| 4. **Filtering cards**: The `Filter` module defines various filters, such as `HasName`, `HasType`, and `CostUpTo`. These filters can be used to narrow down a pile of cards to match specific criteria.
--|
--| 5. **Game logic functions**: The code includes several functions for handling game actions, such as discarding a hand, drawing cards, and gaining cards based on filters. These functions seem to be part of a larger game system.
--|
--| 6. **UI rendering**: Although not explicitly shown in this code snippet, it's likely that the `render` function from the `Game` module is used to display the current game state, including piles, cards, and filters.
--|
--| The code uses several functional programming concepts, such as:
--|
--| * **Monads**: The `Game` module likely uses a monad to manage its state and effects.
--| * **Higher-order functions**: Functions like `parenthesize` and `_stacksToCardsIso` take other functions or values as arguments, allowing for flexible and composable code.
--|
--| Overall, this code appears to be part of a larger game development project that aims to create a modular, reusable, and maintainable game system.
--|
--| ### Key Concepts
--| This appears to be a Haskell code snippet that defines a set of functions and types for working with game-related data structures. I'll break down the main components:
--|
--| 1. **Game data structure**: The `Game` type is defined as a record (similar to an object in other languages) with several fields, including:
--| 	* `_pile`: a field representing the current pile (e.g., hand, discard, etc.)
--| 	* `_stacksToCardsIso`: a function that transforms a stack of cards into an array of cards
--| 2. **Pile data structure**: The `Pile` type is defined as a record with several fields, including:
--| 	* `name`
--| 	* `type`
--| 3. **Stack of cards**: A stack of cards is represented as an `Array` of cards.
--| 4. **Card data structure**: A card has two fields: `name` and `type`.
--| 5. **Filter functions**: Several filter functions are defined, such as `Filter.HasName`, `Filter.HasType`, and `Filter.CostUpTo`. These filters take a name or type parameter and return a boolean indicating whether the card matches.
--| 6. **Card gain functions**: Functions like `GainCards` and `GainCard` update the game state by adding cards to the current pile.
--|
--| Some notable features of this code include:
--|
--| * **Pattern matching**: The code uses pattern matching to destructure data structures, making it easier to work with complex data types.
--| * **Type classes**: Although not explicitly defined in this snippet, type classes are likely used throughout the codebase to define common interfaces for different data structures and functions.
--| * **Higher-order functions**: Functions like `parenthesize` take a single argument and return an array of HTML elements.
--|
--| Overall, this code appears to be part of a larger game development framework, providing a way to manage game-related data structures and perform operations on them.

module Domination.UI.RenderText where

import Prelude

import Data.Array (intercalate)
import Data.Lens ((^?), (^.))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), fromMaybe)
import Domination.Data.Actions (Actions)
import Domination.Data.Actions as Actions
import Domination.Data.Bonus (Bonus(..))
import Domination.Data.Buys (Buys)
import Domination.Data.Buys as Buys
import Domination.Data.Choice (Choice(..))
import Domination.Data.Condition (Condition(..))
import Domination.Data.Constraint (Constraint(..))
import Domination.Data.Filter as Filter
import Domination.Data.Game (Game)
import Domination.Data.Game as Game
import Domination.Data.Phase (Phase(..))
import Domination.Data.Pile (Pile)
import Domination.Data.Pile as Pile
import Domination.Data.Points (Points)
import Domination.Data.Points as Points
import Domination.Data.Reaction (Reaction(..))
import Domination.Data.Result (Result(..))
import Domination.Data.SelectCards (SelectCards(..))
import Domination.Data.Stack (_stacksToCardsIso)
import Domination.Data.Wire.Int as Int
import Domination.UI.Icons as Icons
import Domination.UI.Util (h2__)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Util ((.^))
import Version (Version(..))

class RenderTextInContext a where
  renderTextInContext
    :: forall w i
    . Int
    -> Game
    -> a
    -> HTML w i

class RenderText a where
  renderText :: forall w i. a -> HTML w i

instance resultRenderText :: RenderText Result where
  renderText result = h2__ case result of
    Victory playerIndex ->
      "Victory: Player " <> show (playerIndex + one)
    Tie playerIndices ->
      "Tie: Players " <> (intercalate ", " $ (_ + one) >>> show <$> playerIndices)

instance versionRenderText :: RenderText Version where
  renderText (Version major minor patch) = HH.text $ "v"
    <> (intercalate "." $ show <$> [ major, minor, patch ])

instance phaseRenderText :: RenderText Phase where
  renderText  = case _ of
    ActionPhase -> Icons.actions
    BuyPhase -> Icons.buys
    CleanupPhase -> Icons.cards

instance reactionRenderText :: RenderText Reaction where
  renderText = case _ of
    BlockAttack -> HH.text
      $ "When another player plays an Attack card"
      <> ", you may first reveal this from your hand, to be unaffected by it."

instance bonusRenderText :: RenderText Bonus where
  renderText bonus = HH.span_ case bonus of
    Cash n ->
      [ HH.text $ "+" <> show n
      , Icons.money
      ]

instance actionsRenderText :: RenderText Actions where
  renderText actions = HH.span_
    [ HH.text $ show (actions ^. Actions._int)
    , Icons.actions
    ]

instance buysRenderText :: RenderText Buys where
  renderText buy = HH.span_
    [ HH.text $ show (buy ^. Buys._int)
    , Icons.buys
    ]

instance victoryPointsRenderText :: RenderText Points where
  renderText points = HH.span_
    [ HH.text $ show (points ^. Points._int)
    , Icons.points
    ]

instance conditionRenderText :: RenderText Condition where
  renderText condition = HH.text $ case condition of
    HasCard name -> "hand contains " <> name
    HasCardType cardType -> "hand contains a card of type "
      <> show cardType
    HasDiscard -> "discard pile is not empty"
    DiscardContains name -> "discard contains " <> name
    DiscardContainsCardType cardType -> "discard contains a card of type "
      <> show cardType
    TrashContainsCardType cardType -> "trash contains a card of type "
      <> show cardType
    Randomly percent -> "randomly (" <> percentString <> "% chance)"
      where
        percentString = show $ percent .^ Int._toWire

instance choiceRenderTextInContext
  :: RenderTextInContext Choice where
  renderTextInContext playerIndex state c =
    HH.span_ case c of
      StackChoice { description } -> [ HH.text $ description ]

      If { choice, condition } ->
        [ HH.text "if "
        , renderText condition
        , HH.text " then "
        , renderText choice
        ]

      And { choices } ->
        intercalate [HH.text " and "]
          (pure <<< renderText <$> choices)

      Or { choices } ->
        [ HH.text "chose one of: " ]
          <> (intercalate [ HH.text ", " ]
          $ (pure <<< renderText) <$> choices)

      PickN { n, choices } ->
        [ HH.text $ "chose " <> show n <> " of: " ]
          <> (intercalate [ HH.text ", " ]
          $ (pure <<< renderText) <$> choices)

      Option { choice } ->
        [ HH.text "chose to optionally ", renderText choice ]

      MoveFromTo
        { source, destination, resolution: (Just cardIndices) } ->
        case destination of
          Pile.Supply -> [ HH.text $ "returned to supply: " ] <> cards
          Pile.Trash -> [ HH.text $ "trashed: " ] <> cards
          Pile.Discard -> [ HH.text $ "discarded: " ] <> cards
          Pile.Discarding -> [ HH.text $ "will discard: " ] <> cards
          Pile.Hand -> gainedCardsToTheir "hand"
          Pile.Deck -> gainedCardsToTheir "deck"
          Pile.AtPlay -> [ HH.text $ "played: " ] <> cards
          Pile.Buying -> gainedCardsToTheir "buying pile"
        where
          gainedCardsToTheir pile = [ HH.text $ "gained: " ]
            <> cards
            <> [ HH.text $ " to their " <> pile ]
          cards = intercalate [ HH.text ", " ]
            $ pure
            <<< getCardName source playerIndex state
            <$> cardIndices

      GainCards { n, cardName } ->
        [ HH.text $ "gain a card " <> show n <> "x " <> cardName ]

      GainCard { destination, resolution: Just cardName } ->
        let
          suffix = case destination of
            Pile.AtPlay -> " to play"
            Pile.Hand -> " to their hand"
            Pile.Discard -> " to their discard pile"
            Pile.Discarding -> " to their discarding pile"
            Pile.Buying -> " to their buying pile"
            Pile.Deck -> " onto their deck"
            Pile.Trash -> " into the trash for some reason"
            Pile.Supply -> " into the supply for some reason"
        in [ HH.text $ "gained a " <> cardName <> " " <> suffix ]

      GainActions { n } -> [ HH.text "+", renderText n ]

      GainBuys { n } ->
        [ HH.text "gained +", renderText n ]

      GainBonus { bonus } ->
        [ HH.text "gained", renderText bonus ]

      Discard { selection: SelectAll } ->
        [ HH.text "discarded their hand" ]

      Draw { n } ->
        [ HH.text $ "+" <> show n, Icons.cards ]

      MoveFromTo { resolution: Nothing } -> [ unresolved ]

      GainCard { resolution: Nothing } -> [ unresolved ]
      where
        unresolved :: forall w i. HTML w i
        unresolved = HH.text $ "unresolved choice? (" <> show c <> ")"

instance choiceRenderText :: RenderText Choice where
  renderText choice = HH.span_ case choice of
    StackChoice { description } -> [ HH.text $ description ]

    If { condition, choice: choice', otherwise } ->
      [ HH.text "If ("
      , renderText condition
      , HH.text ") then ("
      , renderText choice'
      , HH.text ")"
      ] <> case otherwise of
        Nothing -> []
        Just o ->
          [ HH.text " otherwise (", renderText o, HH.text ")" ]

    And { choices } ->
      intercalate [ HH.text " and " ]
        (parenthesize <<< renderText <$> choices)

    Or { choices } ->
      intercalate [HH.text " or " ]
        (parenthesize <<< renderText <$> choices)

    PickN { n, choices } ->
      [ HH.text $ show n <> " of: " ]
        <> intercalate [ HH.text " or " ]
        (pure <<< renderText <$> choices)

    Option { choice: choice' } ->
      [ HH.text "optionally ", renderText choice' ]

    MoveFromTo { n, filter, source, destination } -> case n of
      UpTo limit -> [ makeText $ "up to " <> show limit ]
      Exactly limit -> [ makeText $ "exactly " <> show limit ]
      DownTo limit -> [ makeText $ "down to " <> show limit ]
      Unlimited -> [ makeText "any number of" ]
      where
        makeText condition = HH.text $ intercalate " "
          [ verb, condition, description filter, suffix ]
        description = case _ of
          Filter.HasName name -> name
          Filter.HasType cardType -> show cardType
          Filter.CostUpTo cost -> "cards costing up to " <> show cost
          Filter.Any -> "card(s)"
          Filter.And f1 f2 -> description f1 <> " and " <> description f2
        suffix = case source of
          Pile.AtPlay -> "from at play"
          Pile.Hand -> "from your hand"
          Pile.Discard -> "from your discard pile"
          Pile.Buying -> "from your buying pile"
          Pile.Discarding -> "from your to-discard pile"
          Pile.Deck -> "from your deck"
          Pile.Trash -> "from the trash"
          Pile.Supply -> "from the supply"
        verb = case destination of
          Pile.AtPlay -> "Play"
          Pile.Hand -> "Gain to your hand"
          Pile.Discard -> "Discard"
          Pile.Buying -> "Buy"
          Pile.Discarding -> "Discard"
          Pile.Deck -> "Put onto your deck"
          Pile.Trash -> "Trash"
          Pile.Supply -> "Put into the supply"

    GainCards { n, cardName } ->
      [ HH.text $ "Gain " <> cardName <> " x" <> show n ]

    GainCard { filter, destination } ->
      let
        verb = case destination of
          Pile.AtPlay -> "Play"
          Pile.Hand -> "Gain to your hand"
          Pile.Discard -> "Gain"
          Pile.Buying -> "Buy"
          Pile.Discarding -> "Gain"
          Pile.Deck -> "Gain onto your deck"
          Pile.Trash -> "Trash"
          Pile.Supply -> "Put into the supply"
        card = case _ of
          Filter.HasName name -> "1 " <> name
          Filter.HasType cardType -> "a card of type " <> show cardType
          Filter.CostUpTo cost -> "cards costing up to " <> show cost
          Filter.Any -> "a card"
          Filter.And f1 f2 -> card f1 <> " and " <> card f2
      in [ HH.text $ verb <> " " <> card filter <> " from the supply" ]

    GainActions { n } -> [ HH.text "+", renderText n ]

    GainBuys { n } -> [ HH.text "+", renderText n ]

    GainBonus { bonus } -> [ renderText bonus ]

    Discard { selection: SelectAll } ->
      [ HH.text "Discard your hand" ]

    Draw { n } -> [ HH.text $ "+" <> show n, Icons.cards ]

getCardName
  :: forall w i
  . Pile
  -> Int
  -> Game
  -> Int
  -> HTML w i
getCardName source playerIndex state cardIndex = HH.text
  $ fromMaybe "???"
  $ _.name
  <$> state
  ^? Game._pile source playerIndex <<< _stacksToCardsIso
  <<< ix cardIndex

parenthesize :: forall w i. HTML w i -> Array (HTML w i)
parenthesize s = [ HH.text "(", s, HH.text ")" ]
