module Domination.UI.RenderText where

import Prelude

import Data.Array (intercalate)
import Data.Lens ((^?), (^.))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), fromMaybe)
import Domination.Data.Bonus (Bonus(..))
import Domination.Data.Buy (Buy(..))
import Domination.Data.Buy as Buy
import Domination.Data.Choice (Choice(..))
import Domination.Data.Condition (Condition(..))
import Domination.Data.Constraint (Constraint(..))
import Domination.Data.GameState (GameState)
import Domination.Data.GameState as GameState
import Domination.Data.Phase (Phase(..))
import Domination.Data.Pile (Pile)
import Domination.Data.Pile as Pile
import Domination.Data.Reaction (Reaction(..))
import Domination.Data.SelectCards (SelectCards(..))
import Domination.Data.WireInt (_WireInt)
import Domination.UI.Icons as Icons
import Halogen.HTML (HTML(..))
import Halogen.HTML as HH
import Util ((.^))

class RenderTextInContext a where
  renderTextInContext
    :: forall w i
    . Int
    -> GameState
    -> a
    -> HTML w i

class RenderText a where
  renderText :: forall w i. a -> HTML w i

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
        [ HH.text $ "+ " <> show n
        , Icons.actions
        ]
      GainBuys { n, resolution } ->
        [ HH.text $ "gained +" <> show n
        , Icons.buys
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
    MoveFromTo { n, destination } -> case n of
      UpTo n ->
        [ HH.text $ verb <> " up to "
          <> show (n .^ _WireInt)
        ]
      Exactly n ->
        [ HH.text $ verb <> " "
          <> show (n .^ _WireInt)
        ]
      DownTo n ->
        [ HH.text $ verb <> " down to "
          <> show (n .^ _WireInt)
        ]
      where
        verb = case destination of
          Pile.Hand -> "Gain to your hand"
          Pile.Discard -> "Discard"
          Pile.Deck -> "Put onto your deck"
          Pile.Trash -> "Trash"
    GainCards { n, cardName } ->
      [ HH.text $ "Gain " <> cardName <> " x"
        <> show n
      ]
    GainActions { n } ->
      [ HH.text $ "+" <> show n
      , Icons.actions
      ]
    GainBuys { n } ->
      [ HH.text $ "+" <> show n
      , Icons.buys
      ]
    GainBonus { bonus } -> [ renderText bonus ]
    Discard { selection: SelectAll } ->
      [ HH.text "Discard your hand" ]
    Draw { n } ->
      [ HH.text $ "+" <> show n
      , Icons.cards
      ]

instance renderTextBuy :: RenderText Buy where
  renderText buy = HH.span_
    [ HH.text $ show (buy ^. Buy._int)
    , Icons.buys
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
