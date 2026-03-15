--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| A PureScript module for generating HTML for card games.
--|
--| ### Key Concepts
--| * **Game Logic**: The module implements game logic for a card-based game.
--| * **Pile Management**: It manages cards in different piles (e.g., hands, discard pile, deck).
--| * **Card Interactions**: It defines how cards interact with each other and the player's actions (e.g., drawing, playing).

module Domination.UI.RenderText where

import Prelude

import Data.Array (intercalate)
import Data.Lens ((^?), (^.))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), snd)
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
    ReactWithChoice _ -> HH.text "React with a choice"

instance tupleReactionStringRenderText :: RenderText (Tuple Reaction String) where
  renderText = HH.text <<< snd

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
