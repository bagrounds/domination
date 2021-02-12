module Domination.Data.Cards where

import Prelude

import Data.Array (findIndex, (!!))
import Data.Lens.Getter (view, (^.))
import Data.Lens.Iso (Iso', iso)
import Data.Lens.Prism (review)
import Data.Maybe (Maybe(..), fromMaybe)
import Domination.Data.Bonus (Bonus(..))
import Domination.Data.Card (Card, Command(..), Special)
import Domination.Data.Card as Card
import Domination.Data.CardType (CardType(..))
import Domination.Data.Choice (Choice(..))
import Domination.Data.Condition (Condition(..))
import Domination.Data.Constraint (Constraint(..))
import Domination.Data.Filter (Filter(..))
import Domination.Data.Pile as Pile
import Domination.Data.Reaction (Reaction(..))
import Domination.Data.SelectCards (SelectCards(..))
import Domination.Data.Target (Target(..))
import Domination.Data.WireInt (WireInt, _WireInt)

_toWire :: Iso' Card WireInt
_toWire = iso to from where
  to card = view _WireInt
    <<< fromMaybe (-1) $ findIndex (_ == card) cardMap
  from = fromMaybe Card.card
    <<< (cardMap !! _)
    <<< review _WireInt

cardMap :: Array Card
cardMap =
  [ copper
  , silver
  , gold
  , platinum
  , estate
  , duchy
  , province
  , colony
  , curse
  , chapel
  , moat
  , pawn
  , consolation
  , greatHall
  , village
  , woodCutter
  , steward
  , harbinger
  , goldfish
  , baron
  , monument
  , smithy
  , workersVillage
  , militia
  , moneyLender
  , bazaar
  , festival
  , laboratory
  , market
  , witch
  , councilRoom
  , scholar
  , torturer
  , mountebank
  , harem
  , nobles
  ]

emptyChoice :: Choice
emptyChoice = GainBonus
  { bonus: Cash $ 100 ^. _WireInt
  , attack: false
  , resolution: Nothing
  }

copper :: Card
copper = Card.treasure { name = "Copper", treasure = 1 }

silver :: Card
silver = Card.treasure { name = "Silver", cost = 3, treasure = 2 }

gold :: Card
gold = Card.treasure { name = "Gold", cost = 6, treasure = 3 }

platinum :: Card
platinum = Card.treasure { name = "Platinum", cost = 9, treasure = 5 }

estate :: Card
estate = Card.victory { name = "Estate", cost = 2, victoryPoints  = 1 }

duchy :: Card
duchy = Card.victory { name = "Duchy", cost = 5, victoryPoints = 3 }

province :: Card
province =
  Card.victory { name = "Province", cost = 8, victoryPoints = 6 }

colony :: Card
colony =
  Card.victory { name = "Colony", cost = 11, victoryPoints = 10 }

curse :: Card
curse =
  Card.card { types = [Curse], name = "Curse", victoryPoints = -1 }

greatHall :: Card
greatHall = Card.victory
  { types = [Action, Victory]
  , name = "Great Hall"
  , cost = 3
  , cards = 1
  , actions = 1
  , victoryPoints = 1
  }

village :: Card
village =
  Card.action { name = "Village", cost = 3, cards = 1, actions = 2 }

woodCutter :: Card
woodCutter = Card.action
  { name = "Wood Cutter"
  , cost = 3
  , buys = 1
  , treasure = 2
  }

laboratory :: Card
laboratory = Card.action
  { name = "Laboratory"
  , cost = 5
  , cards = 2
  , actions = 1
  }

smithy :: Card
smithy = Card.action { name = "Smithy", cost = 4, cards = 3 }

festival :: Card
festival = Card.action
  { name = "Festival"
  , cost = 5
  , actions = 2
  , buys = 1
  , treasure = 2
  }

market :: Card
market = Card.action
  { name = "Market"
  , cost = 5
  , actions = 1
  , cards = 1
  , buys = 1
  , treasure = 1
  }

harem :: Card
harem = Card.treasure
  { types = [Treasure, Victory]
  , name = "Harem"
  , cost = 6
  , treasure = 2
  , victoryPoints = 2
  }

bazaar :: Card
bazaar = Card.action
  { name = "Bazaar"
  , cost = 5
  , cards = 1
  , actions = 2
  , treasure = 1
  }

monument :: Card
monument = Card.action
  { types = [Action, Victory]
  , name = "Monument"
  , cost = 4
  , treasure = 2
  , victoryPoints = 1
  }

workersVillage :: Card
workersVillage = Card.action
  { name = "Worker's Village"
  , cost = 4
  , cards = 1
  , actions = 2
  , buys = 1
  }

witch :: Card
witch = let attack = true in
  Card.actionAttack
  { name = "Witch"
  , cost = 5
  , cards = 2
  , special = Just witchSpecial
  }

witchChoice :: Choice
witchChoice = let attack = true in GainCards
  { cardName: "Curse"
  , destination: Pile.Discard
  , n: 1
  , resolution: Nothing
  , attack
  }

witchSpecial :: Special
witchSpecial = let attack = true in
  { target: EveryoneElse
  , command: Choose witchChoice
  , description: "Each other player gains a Curse."
  }

councilRoom :: Card
councilRoom = let attack = false in
  Card.action
  { name = "Council Room"
  , cost = 5
  , cards = 4
  , buys = 1
  , special = Just councilRoomSpecial
  }

councilRoomChoice :: Choice
councilRoomChoice = let attack = false in Draw
  { n: 1
  , resolution: Nothing
  , attack
  }

councilRoomSpecial :: Special
councilRoomSpecial = let attack = false in
  { target: EveryoneElse
  , command: Choose councilRoomChoice
  , description: "Each other player draws a card."
  }

scholar :: Card
scholar = let attack = false in
  Card.action
  { name = "Scholar"
  , cost = 5
  , special = Just scholarSpecial
  }

scholarChoice :: Choice
scholarChoice = let attack = false in And
  { choices:
    [ Discard
      { selection: SelectAll
      , resolution: Nothing
      , attack
      }
    , Draw
      { n: 7
      , resolution: Nothing
      , attack
      }
    ]
    , resolution: Nothing
    , attack
  }

scholarSpecial :: Special
scholarSpecial = let attack = false in
  { target: Self
  , command: Choose scholarChoice
  , description: "Discard your hand and draw 7 cards"
  }

chapel :: Card
chapel = let attack = false in
  Card.action
  { name = "Chapel"
  , cost = 2
  , special = Just chapelSpecial
  }

chapelChoice :: Choice
chapelChoice = let attack = false in MoveFromTo
  { n: UpTo $ 4 ^. _WireInt
  , filter: Nothing
  , source: Pile.Hand
  , destination: Pile.Trash
  , resolution: Nothing
  , attack
  }

chapelSpecial :: Special
chapelSpecial = let attack = false in
  { target: Self
  , command: Choose chapelChoice
  , description: "Trash up to 4 cards from your hand"
  }

militia :: Card
militia = let attack = true in
  Card.actionAttack
  { name = "Militia"
  , cost = 4
  , treasure = 2
  , special = Just militiaSpecial
  }

militiaChoice :: Choice
militiaChoice = let attack = true in MoveFromTo
  { n: DownTo $ 3 ^. _WireInt
  , filter: Nothing
  , source: Pile.Hand
  , destination: Pile.Discard
  , resolution: Nothing
  , attack
  }

militiaSpecial :: Special
militiaSpecial = let attack = true in
  { target: EveryoneElse
  , command: Choose militiaChoice
  , description: "Each other player discards down to 3 cards"
  }

moat :: Card
moat =
  Card.actionReaction
  { name = "Moat"
  , cost = 2
  , cards = 2
  , reaction = Just BlockAttack
  }

nobles :: Card
nobles = let attack = false in
  Card.actionVictory
  { name = "Nobles"
  , cost = 6
  , victoryPoints = 2
  , special = Just noblesSpecial
  }

noblesChoice :: Choice
noblesChoice = let attack = false in Or
  { choices:
    [ Draw { n: 3, attack, resolution: Nothing }
    , GainActions { n: 2, attack, resolution: Nothing }
    ]
  , resolution: Nothing
  , attack
  }

noblesSpecial :: Special
noblesSpecial = let attack = false in
  { target: Self
  , command: Choose noblesChoice
  , description: "Choose one: +3 cards or +2 actions"
  }

steward :: Card
steward = let attack = false in
  Card.action
  { name = "Steward"
  , cost = 3
  , special = Just stewardSpecial
  }

stewardChoice :: Choice
stewardChoice = let attack = false in Or
  { choices:
    [ Draw { n: 2, attack, resolution: Nothing }
    , GainBonus { bonus: Cash $ 2 ^. _WireInt, attack, resolution: Nothing }
    , MoveFromTo
      { source: Pile.Hand
      , destination: Pile.Trash
      , filter: Nothing
      , n: Exactly $ 2 ^. _WireInt
      , attack
      , resolution: Nothing
      }
    ]
  , resolution: Nothing
  , attack
  }

stewardSpecial :: Special
stewardSpecial = let attack = false in
  { target: Self
  , command: Choose stewardChoice
  , description:
    "Choose 1: + 2 cards, + $2, or trash 2 cards from your hand"
  }

pawn :: Card
pawn = Card.action
  { name = "Pawn"
  , cost = 2
  , special = Just pawnSpecial
  }

pawnChoice :: Choice
pawnChoice = let attack = false in PickN
  { n: 2
  , choices:
    [ Draw { n: 1, attack, resolution: Nothing }
    , GainBonus { bonus: Cash $ 1 ^. _WireInt, attack, resolution: Nothing }
    , GainActions { n: 1, attack, resolution: Nothing }
    , GainBuys { n: 1, attack, resolution: Nothing }
    ]
  , resolution: Nothing
  , attack
  }

pawnSpecial :: Special
pawnSpecial = let attack = false in
  { target: Self
  , command: Choose pawnChoice
  , description: "Choose 2 of: + $1, + 1 card, + 1 action, or +1 buy"
  }

torturer :: Card
torturer = Card.actionAttack
  { name = "Torturer"
  , cost = 5
  , cards = 3
  , special = Just torturerSpecial
  }

torturerChoice :: Choice
torturerChoice = let attack = true in
  PickN
  { n: 1
  , choices:
    [ MoveFromTo
      { n: Exactly $ 2 ^. _WireInt
      , filter: Nothing
      , source: Pile.Hand
      , destination: Pile.Discard
      , attack
      , resolution: Nothing
      }
    , GainCards
      { n: 1
      , cardName: "Curse"
      , destination: Pile.Discard
      , attack
      , resolution: Nothing
      }
    ]
  , resolution: Nothing
  , attack
  }

torturerSpecial :: Special
torturerSpecial = let attack = true in
  { target: EveryoneElse
  , command: Choose torturerChoice
  , description:
    "Each other player either discards 2 cards or gains a Curse to their hand, their choice. (They may pick an option they can't do.)"
  }

consolation :: Card
consolation = Card.action
  { name = "Consolation"
  , cost = 2
  , special = Just consolationSpecial
  }

consolationChoice :: Choice
consolationChoice = let attack = false in
  If
  { condition: HasCard "Estate"
  , choice: GainBonus
    { bonus: Cash $ 2 ^. _WireInt
    , attack
    , resolution: Nothing
    }
  , otherwise: Nothing
  , resolution: Nothing
  , attack
  }

consolationSpecial :: Special
consolationSpecial =
  { target: Self
  , command: Choose consolationChoice
  , description: "If you have an Estate in your hand, + $2"
  }

moneyLender :: Card
moneyLender = Card.action
  { name = "Money Lender"
  , cost = 4
  , special = Just moneyLenderSpecial
  }

moneyLenderChoice :: Choice
moneyLenderChoice = let attack = false in
  If
  { condition: HasCard "Copper"
  , choice: Option
    { choice: And
      { choices:
        [ MoveFromTo
          { n: Exactly $ 1 ^. _WireInt
          , filter: Just (HasName "Copper")
          , source: Pile.Hand
          , destination: Pile.Trash
          , attack
          , resolution: Nothing
          }
        , GainBonus
          { bonus: Cash $ 3 ^. _WireInt
          , attack
          , resolution: Nothing
          }
        ]
      , attack
      , resolution: Nothing
      }
    , attack
    , resolution: Nothing
    }
  , otherwise: Nothing
  , attack
  , resolution: Nothing
  }

moneyLenderSpecial :: Special
moneyLenderSpecial = let attack = false in
  { target: Self
  , command: Choose moneyLenderChoice
  , description: "You may trash a copper from your hand for + $3"
  }

harbinger :: Card
harbinger = Card.action
  { name = "Harbinger"
  , cost = 3
  , cards = 1
  , actions = 1
  , special = Just harbingerSpecial
  }

harbingerChoice :: Choice
harbingerChoice = MoveFromTo
  { n: UpTo $ 1 ^. _WireInt
  , filter: Nothing
  , source: Pile.Discard
  , destination: Pile.Deck
  , resolution: Nothing
  , attack: false
  }

harbingerSpecial :: Special
harbingerSpecial =
  { target: Self
  , command: Choose $ harbingerChoice
  , description: "Look through your discard pile."
    <> " You may put a card from it onto your deck."
  }

baron :: Card
baron = Card.action
  { name = "Baron"
  , cost = 4
  , buys = 1
  , special = Just baronSpecial
  }

gain4Cash :: Choice
gain4Cash = GainBonus
  { bonus: Cash $ 4 ^. _WireInt
  , attack: false
  , resolution: Nothing
  }

gain1Estate :: Choice
gain1Estate = GainCards
  { cardName: "Estate"
  , n: 1
  , destination: Pile.Discard
  , attack: false
  , resolution: Nothing
  }

discard1Estate :: Choice
discard1Estate = MoveFromTo
  { filter: Just $ HasName "Estate"
  , n: Exactly $ 1 ^. _WireInt
  , source: Pile.Hand
  , destination: Pile.Discard
  , attack: false
  , resolution: Nothing
  }

discard1EstateAndGain4Cash :: Choice
discard1EstateAndGain4Cash = And
  { choices: [ gain4Cash, discard1Estate ]
  , attack: false
  , resolution: Nothing
  }

discardOrGain1Estate :: Choice
discardOrGain1Estate = Or
  { choices: [ gain1Estate, discard1EstateAndGain4Cash ]
  , attack: false
  , resolution: Nothing
  }

baronChoice :: Choice
baronChoice = If
  { condition: HasCard "Estate"
  , choice: discardOrGain1Estate
  , otherwise: Just gain1Estate
  , attack: false
  , resolution: Nothing
  }

baronSpecial :: Special
baronSpecial =
  { target: Self
  , command: Choose baronChoice
  , description: "You may discard an estate for + $4."
    <> " If you don't, gain an Estate."
  }

goldfish :: Card
goldfish = Card.action
  { name = "Goldfish"
  , cost = 4
  , buys = 1
  , special = Just goldfishSpecial
  }

goldfishSpecial :: Special
goldfishSpecial =
  { target: Self
  , command: Choose goldfishChoice
  , description: "50% chance to gain a gold to your hand"
  }

goldfishChoice :: Choice
goldfishChoice = let attack = false in If
  { condition: Randomly $ 50 ^. _WireInt
  , choice: GainCards
    { cardName: "Gold"
    , destination: Pile.Hand
    , n: 1
    , resolution: Nothing
    , attack
    }
  , otherwise: Nothing
  , attack
  , resolution: Nothing
  }

mountebank :: Card
mountebank = Card.action
  { name = "Mountebank"
  , cost = 5
  , treasure = 2
  , special = Just
    { target: EveryoneElse
    , command: Choose mountebankChoice
    , description: "Each other player may discard a Curse."
      <> "If they don't, they gain a Curse and a Copper."
    }
  }

mountebankChoice :: Choice
mountebankChoice = let attack = true in If
  { condition: HasCard "Curse"
  , choice: Or
    { choices:
      [ MoveFromTo
        { n: Exactly $ 1 ^. _WireInt
        , filter: Just $ HasName "Curse"
        , source: Pile.Hand
        , destination: Pile.Discard
        , attack
        , resolution: Nothing
        }
      , gainCurseAndCopper
      ]
    , attack
    , resolution: Nothing
    }
  , attack
  , resolution: Nothing
  , otherwise: Just gainCurseAndCopper
  }

gainCurseAndCopper :: Choice
gainCurseAndCopper = let attack = true in And
  { choices:
    [ GainCards
      { cardName: "Copper"
      , destination: Pile.Discard
      , n: 1
      , attack
      , resolution: Nothing
      }
    , GainCards
      { cardName: "Curse"
      , destination: Pile.Discard
      , n: 1
      , attack
      , resolution: Nothing
      }
    ]
  , attack
  , resolution: Nothing
  }

