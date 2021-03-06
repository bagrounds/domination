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
import Domination.Data.Filter (Filter(..))
import Domination.Data.GameState (GameState)
import Domination.Data.GameState as GameState
import Domination.Data.Phase (Phase(..))
import Domination.Data.Pile (Pile)
import Domination.Data.Pile as Pile
import Domination.Data.Points (Points)
import Domination.Data.Points as Points
import Domination.Data.Reaction (Reaction(..))
import Domination.Data.Result (Result(..))
import Domination.Data.SelectCards (SelectCards(..))
import Domination.Data.WireInt (_WireInt)
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
    -> GameState
    -> a
    -> HTML w i

class RenderText a where
  renderText :: forall w i. a -> HTML w i

instance resultRenderText :: RenderText Result where
  renderText result = h2__ case result of
    Victory playerIndex ->
      "Victory: Player " <> show (playerIndex + one)
    Tie playerIndices ->
      "Tie: Players " <> (intercalate ", " $ show <$> playerIndices)

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
      [ HH.text $ "+" <> show (n .^ _WireInt)
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
    HasDiscard -> "discard pile is not empty"
    Randomly percent -> "randomly (" <> percentString <> "% chance)"
      where
        percentString = show $ percent .^ _WireInt

instance choiceRenderTextInContext
  :: RenderTextInContext Choice where
  renderTextInContext playerIndex state c =
    HH.span_ case c of
      If { choice, condition } ->
        [ HH.text "if "
        , renderText condition
        , HH.text " then "
        , renderText choice
        ]
      And { choices } ->
        intercalate [HH.text " and "] (pure <<< renderText <$> choices)
      Or { choices } ->
        [ HH.text "chose one of: " ]
          <> (intercalate [ HH.text ", " ]
          $ (pure <<< renderText) <$> choices)
      PickN { n, choices, resolution } ->
        [ HH.text $ "chose " <> show n <> " of: " ]
          <> (intercalate [ HH.text ", " ]
          $ (pure <<< renderText) <$> choices)
      Option { choice } ->
        [ HH.text "chose to optionally "
        , renderText choice
        ]
      MoveFromTo { source, destination, resolution: (Just cardIndices) } ->
        case destination of
            Pile.Trash ->
              [ HH.text $ "trashed: " ] <> cards
            Pile.Discard ->
              [ HH.text $ "discarded: " ] <> cards
            Pile.ToDiscard ->
              [ HH.text $ "will discard: " ] <> cards
            Pile.Hand ->
              [ HH.text $ "gained: " ]
                <> cards
                <> [ HH.text " to their hand" ]
            Pile.Deck ->
              [ HH.text $ "gained: " ]
                <> cards
                <> [ HH.text " to their deck" ]
        where
          cards = intercalate [ HH.text ", " ]
            $ pure
            <<< getCardName source playerIndex state
            <$> cardIndices
      GainCards { n, cardName } ->
        [ HH.text $ "+" <> show n <> "x " <> cardName ]
      GainActions { n, resolution } ->
        [ HH.text "+"
        , renderText n
        ]
      GainBuys { n, resolution } ->
        [ HH.text "gained +"
        , renderText n
        ]
      GainBonus { bonus, resolution } ->
        [ HH.text "gained"
        , renderText bonus
        ]
      Discard { selection: SelectAll } ->
        [ HH.text "discarded their hand" ]
      Draw { n, resolution } ->
        [ HH.text $ "+" <> show n
        , Icons.cards
        ]
      MoveFromTo { resolution: Nothing } ->
        [ unresolved ]
      where
        unresolved :: forall w i. HTML w i
        unresolved = HH.text
          $ "unresolved choice? (" <> show c <> ")"

instance choiceRenderText :: RenderText Choice where
  renderText choice = HH.span_ case choice of
    If { condition, choice, otherwise } ->
      [ HH.text "If ("
      , renderText condition
      , HH.text ") then ("
      , renderText choice
      , HH.text ")"
      ] <> case otherwise of
        Nothing -> []
        Just o ->
          [ HH.text " otherwise ("
          , renderText o
          , HH.text ")"
          ]
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
    Option { choice } ->
      [ HH.text "optionally "
      , renderText choice
      ]
    MoveFromTo { n, filter, destination } -> case n of
      UpTo n ->
        [ HH.text $ verb <> " up to "
          <> show (n .^ _WireInt)
          <> suffix
        ]
      Exactly n ->
        [ HH.text $ verb <> " "
          <> show (n .^ _WireInt)
          <> suffix
        ]
      DownTo n ->
        [ HH.text $ verb <> " down to "
          <> show (n .^ _WireInt)
          <> suffix
        ]
      where
        suffix = case filter of
          Nothing -> " cards"
          Just (HasName name) -> " " <> name

        verb = case destination of
          Pile.Hand -> "Gain to your hand"
          Pile.Discard -> "Discard"
          Pile.ToDiscard -> "Discard"
          Pile.Deck -> "Put onto your deck"
          Pile.Trash -> "Trash"
    GainCards { n, cardName } ->
      [ HH.text $ "Gain " <> cardName <> " x"
        <> show n
      ]
    GainActions { n } ->
      [ HH.text "+"
      , renderText n
      ]
    GainBuys { n } ->
      [ HH.text "+"
      , renderText n
      ]
    GainBonus { bonus } -> [ renderText bonus ]
    Discard { selection: SelectAll } ->
      [ HH.text "Discard your hand" ]
    Draw { n } ->
      [ HH.text $ "+" <> show n
      , Icons.cards
      ]

getCardName
  :: forall w i
  . Pile
  -> Int
  -> GameState
  -> Int
  -> HTML w i
getCardName source playerIndex state cardIndex = HH.text
  $ fromMaybe "???"
  $ _.name
  <$> state
  ^? GameState._pile source playerIndex
  <<< ix cardIndex

parenthesize s = [ HH.text "(", s, HH.text ")" ]
