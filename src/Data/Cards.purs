module Domination.Data.Cards where

import Prelude

import Data.Foldable (find)
import Data.Lens.Getter ((^.))
import Data.Maybe (Maybe(..), fromMaybe)
import Domination.Data.Actions (actions)
import Domination.Data.Bonus (Bonus(..))
import Domination.Data.Card (Card, CardSpec, Command(..), Special, cardWithRequirements, independentCard)
import Domination.Data.Card as Card
import Domination.Data.CardType (CardType(..))
import Domination.Data.Choice (Choice(..))
import Domination.Data.Condition (Condition(..))
import Domination.Data.Constraint (Constraint(..))
import Domination.Data.Filter as Filter
import Domination.Data.Pile as Pile
import Domination.Data.Points (points)
import Domination.Data.Reaction (Reaction(..))
import Domination.Data.SelectCards (SelectCards(..))
import Domination.Data.StackEvaluation (StackExpression(..), StackValue(..))
import Domination.Data.Target (Target(..))
import Domination.Data.Var (Var(..))
import Domination.Data.Wire.Int as Int

upgrade :: Card -> Card
upgrade card = fromMaybe card
  $ find ((_.name >>> (_ == card.name))) cardMap

cardMap :: Array Card
cardMap = Card._card <$> cardSpecMap

cardSpecMap :: Array CardSpec
cardSpecMap =
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
  , settlers
  , courtyard
  , lurker
  , cellar
  , greatHall
  , village
  , woodCutter
  , steward
  , harbinger
  , workshop
  , mill
  , goldfish
  , baron
  , monument
  , smithy
  , workersVillage
  , militia
  , moneyLender
  , armory
  , remodel
  , innkeeper
  , catpurse
  , bazaar
  , festival
  , laboratory
  , market
  , witch
  , councilRoom
  , scholar
  , torturer
  , mountebank
  , margrave
  , huntingLodge
  , oldWitch
  , junkDealer
  , stables
  , tradingPost
  , mine
  , harem
  , nobles
  , artisan
  , altar
  , expand
  ]

emptyChoice :: Choice
emptyChoice = GainBonus
  { bonus: Cash 100
  , attack: false
  , resolution
  }

copper :: CardSpec
copper = independentCard $ Card.treasure { name = "Copper", treasure = one }

silver :: CardSpec
silver = independentCard $ Card.treasure { name = "Silver", cost = 3, treasure = 2 }

gold :: CardSpec
gold = independentCard $ Card.treasure { name = "Gold", cost = 6, treasure = 3 }

platinum :: CardSpec
platinum = independentCard $ Card.treasure { name = "Platinum", cost = 9, treasure = 5 }

estate :: CardSpec
estate = independentCard $ Card.victory
  { name = "Estate"
  , cost = 2
  , victoryPoints = one
  }

duchy :: CardSpec
duchy = independentCard $ Card.victory
  { name = "Duchy"
  , cost = 5
  , victoryPoints = points 3
  }

province :: CardSpec
province = independentCard $ Card.victory
  { name = "Province"
  , cost = 8
  , victoryPoints = points 6
  }

colony :: CardSpec
colony = independentCard $ Card.victory
  { name = "Colony"
  , cost = 11
  , victoryPoints = points 10
  }

curse :: CardSpec
curse = independentCard $ 
  Card.card { types = [Curse], name = "Curse", victoryPoints = -one }

greatHall :: CardSpec
greatHall = independentCard $ Card.victory
  { types = [Action, Victory]
  , name = "Great Hall"
  , cost = 3
  , cards = one
  , actions = one
  , victoryPoints = one
  }

village :: CardSpec
village = independentCard $ Card.action
  { name = "Village"
  , cost = 3
  , cards = one
  , actions = actions 2
  }

woodCutter :: CardSpec
woodCutter = independentCard $ Card.action
  { name = "Wood Cutter"
  , cost = 3
  , buys = one
  , treasure = 2
  }

laboratory :: CardSpec
laboratory = independentCard $ Card.action
  { name = "Laboratory"
  , cost = 5
  , cards = 2
  , actions = one
  }

smithy :: CardSpec
smithy = independentCard $ Card.action { name = "Smithy", cost = 4, cards = 3 }

festival :: CardSpec
festival = independentCard $ Card.action
  { name = "Festival"
  , cost = 5
  , actions = actions 2
  , buys = one
  , treasure = 2
  }

market :: CardSpec
market = independentCard $ Card.action
  { name = "Market"
  , cost = 5
  , actions = one
  , cards = one
  , buys = one
  , treasure = one
  }

harem :: CardSpec
harem = independentCard $ Card.treasure
  { types = [Treasure, Victory]
  , name = "Harem"
  , cost = 6
  , treasure = 2
  , victoryPoints = points 2
  }

bazaar :: CardSpec
bazaar = independentCard $ Card.action
  { name = "Bazaar"
  , cost = 5
  , cards = one
  , actions = actions 2
  , treasure = one
  }

monument :: CardSpec
monument = independentCard $ Card.action
  { types = [Action, Victory]
  , name = "Monument"
  , cost = 4
  , treasure = 2
  , victoryPoints = one
  }

workersVillage :: CardSpec
workersVillage = independentCard $ Card.action
  { name = "Worker's Village"
  , cost = 4
  , cards = one
  , actions = actions 2
  , buys = one
  }

witch :: CardSpec
witch = cardWithRequirements c rs
  where
    c :: Card
    c = Card.actionAttack
      { name = "Witch"
      , cost = 5
      , cards = 2
      , special = Just witchSpecial
      }
    rs :: Array CardSpec
    rs = [curse]

gainCurse :: Choice
gainCurse = let attack = true in GainCards
  { cardName: "Curse"
  , destination: Pile.Discard
  , n: one
  , resolution
  , attack
  }

witchSpecial :: Special
witchSpecial =
  { target: EveryoneElse
  , command: Choose gainCurse
  , description: "Each other player gains a Curse."
  }

councilRoom :: CardSpec
councilRoom = independentCard $ Card.action
  { name = "Council Room"
  , cost = 5
  , cards = 4
  , buys = one
  , special = Just councilRoomSpecial
  }

draw1Card :: Choice
draw1Card = let attack = false in Draw
  { n: one
  , resolution
  , attack
  }

councilRoomSpecial :: Special
councilRoomSpecial =
  { target: EveryoneElse
  , command: Choose draw1Card
  , description: "Each other player draws a card."
  }

scholar :: CardSpec
scholar =
  independentCard $ Card.action
  { name = "Scholar"
  , cost = 5
  , special = Just scholarSpecial
  }

discardYourHand :: Boolean -> Choice
discardYourHand attack = Discard
  { selection: SelectAll
  , resolution
  , attack
  }

scholarChoice :: Choice
scholarChoice = let attack = false in And
  { choices:
    [ discardYourHand attack
    , Draw
      { n: 7
      , resolution
      , attack
      }
    ]
    , resolution
    , attack
  }

scholarSpecial :: Special
scholarSpecial =
  { target: Self
  , command: Choose scholarChoice
  , description: "Discard your hand and draw 7 cards"
  }

chapel :: CardSpec
chapel =
  independentCard $ Card.action
  { name = "Chapel"
  , cost = 2
  , special = Just chapelSpecial
  }

chapelChoice :: Choice
chapelChoice = let attack = false in MoveFromTo
  { n: UpTo 4
  , filter: Filter.Any
  , source: Pile.Hand
  , destination: Pile.Trash
  , resolution
  , attack
  }

chapelSpecial :: Special
chapelSpecial =
  { target: Self
  , command: Choose chapelChoice
  , description: "Trash up to 4 cards from your hand"
  }

militia :: CardSpec
militia =
  independentCard $ Card.actionAttack
  { name = "Militia"
  , cost = 4
  , treasure = 2
  , special = Just militiaSpecial
  }

discardDownTo3 :: Choice
discardDownTo3 = let attack = true in MoveFromTo
  { n: DownTo 3
  , filter: Filter.Any
  , source: Pile.Hand
  , destination: Pile.Discard
  , resolution
  , attack
  }

militiaSpecial :: Special
militiaSpecial =
  { target: EveryoneElse
  , command: Choose discardDownTo3
  , description: "Each other player discards down to 3 cards"
  }

moat :: CardSpec
moat =
  independentCard $ Card.actionReaction
  { name = "Moat"
  , cost = 2
  , cards = 2
  , reaction = Just BlockAttack
  }

nobles :: CardSpec
nobles =
  independentCard $ Card.actionVictory
  { name = "Nobles"
  , cost = 6
  , victoryPoints = points 2
  , special = Just noblesSpecial
  }

noblesChoice :: Choice
noblesChoice = let attack = false in Or
  { choices:
    [ Draw { n: 3, attack, resolution }
    , GainActions { n: actions 2, attack, resolution }
    ]
  , resolution
  , attack
  }

noblesSpecial :: Special
noblesSpecial =
  { target: Self
  , command: Choose noblesChoice
  , description: "Choose one: +3 cards or +2 actions"
  }

innkeeper :: CardSpec
innkeeper =
  let attack = false in
  independentCard $ Card.action
  { name = "Innkeeper"
  , cost = 4
  , actions = one
  , special = Just
    { target: Self
    , command: Choose $ Or
      { choices:
        [ Draw { n: 1, attack, resolution }
        , And
          { choices:
            [ Draw { n: 3, attack, resolution }
            , MoveFromTo
              { n: Exactly 3
              , filter: Filter.Any
              , source: Pile.Hand
              , destination: Pile.Discarding
              , attack
              , resolution
              }
            ]
            , resolution
            , attack
          }
        , And
          { choices:
            [ Draw { n: 5, attack, resolution }
            , MoveFromTo
              { n: Exactly 6
              , filter: Filter.Any
              , source: Pile.Hand
              , destination: Pile.Discarding
              , attack
              , resolution
              }
            ]
            , resolution
            , attack
          }
        ]
      , resolution
      , attack
      }
    , description: "Choose one: +1 Card; or +3 Cards, then discard 3 cards; or +5 Cards, then discard 6 cards."
    }
  }

steward :: CardSpec
steward =
  independentCard $ Card.action
  { name = "Steward"
  , cost = 3
  , special = Just stewardSpecial
  }

stewardChoice :: Choice
stewardChoice = let attack = false in Or
  { choices:
    [ Draw { n: 2, attack, resolution }
    , GainBonus { bonus: Cash 2, attack, resolution }
    , MoveFromTo
      { source: Pile.Hand
      , destination: Pile.Trash
      , filter: Filter.Any
      , n: Exactly 2
      , attack
      , resolution
      }
    ]
  , resolution
  , attack
  }

stewardSpecial :: Special
stewardSpecial =
  { target: Self
  , command: Choose stewardChoice
  , description:
    "Choose 1: + 2 cards, + $2, or trash 2 cards from your hand"
  }

pawn :: CardSpec
pawn = independentCard $ Card.action
  { name = "Pawn"
  , cost = 2
  , special = Just pawnSpecial
  }

pawnChoice :: Choice
pawnChoice = let attack = false in PickN
  { n: 2
  , choices:
    [ Draw { n: one, attack, resolution }
    , GainBonus { bonus: Cash one, attack, resolution }
    , GainActions { n: one, attack, resolution }
    , GainBuys { n: one, attack, resolution }
    ]
  , resolution
  , attack
  }

pawnSpecial :: Special
pawnSpecial =
  { target: Self
  , command: Choose pawnChoice
  , description: "Choose 2 of: + $1, + 1 card, + 1 action, or +1 buy"
  }

torturer :: CardSpec
torturer = cardWithRequirements c rs
  where
    c :: Card
    c = Card.actionAttack
      { name = "Torturer"
      , cost = 5
      , cards = 3
      , special = Just torturerSpecial
      }
    rs :: Array CardSpec
    rs = [curse]

torturerChoice :: Choice
torturerChoice = let attack = true in
  PickN
  { n: one
  , choices:
    [ MoveFromTo
      { n: Exactly 2
      , filter: Filter.Any
      , source: Pile.Hand
      , destination: Pile.Discard
      , attack
      , resolution
      }
    , GainCards
      { n: one
      , cardName: "Curse"
      , destination: Pile.Discard
      , attack
      , resolution
      }
    ]
  , resolution
  , attack
  }

torturerSpecial :: Special
torturerSpecial =
  { target: EveryoneElse
  , command: Choose torturerChoice
  , description:
    "Each other player either discards 2 cards or gains a Curse to their hand, their choice. (They may pick an option they can't do.)"
  }

consolation :: CardSpec
consolation = cardWithRequirements c rs
  where
    c :: Card
    c = Card.action
      { name = "Consolation"
      , cost = 2
      , special = Just consolationSpecial
      }
    rs :: Array CardSpec
    rs = [estate]

consolationChoice :: Choice
consolationChoice = let attack = false in
  If
  { condition: HasCard "Estate"
  , choice: GainBonus
    { bonus: Cash 2
    , attack
    , resolution
    }
  , otherwise: Nothing
  , resolution
  , attack
  }

consolationSpecial :: Special
consolationSpecial =
  { target: Self
  , command: Choose consolationChoice
  , description: "If you have an Estate in your hand, + $2"
  }

moneyLender :: CardSpec
moneyLender = cardWithRequirements c rs
  where
    c :: Card
    c = Card.action
      { name = "Money Lender"
      , cost = 4
      , special = Just moneyLenderSpecial
      }
    rs :: Array CardSpec
    rs = [copper]

moneyLenderChoice :: Choice
moneyLenderChoice = let attack = false in
  If
  { condition: HasCard "Copper"
  , choice: Option
    { choice: And
      { choices:
        [ MoveFromTo
          { n: Exactly one
          , filter: Filter.HasName "Copper"
          , source: Pile.Hand
          , destination: Pile.Trash
          , attack
          , resolution
          }
        , GainBonus
          { bonus: Cash 3
          , attack
          , resolution
          }
        ]
      , attack
      , resolution
      }
    , attack
    , resolution
    }
  , otherwise: Nothing
  , attack
  , resolution
  }

moneyLenderSpecial :: Special
moneyLenderSpecial =
  { target: Self
  , command: Choose moneyLenderChoice
  , description: "You may trash a copper from your hand for + $3"
  }

harbinger :: CardSpec
harbinger = independentCard $ Card.action
  { name = "Harbinger"
  , cost = 3
  , cards = one
  , actions = one
  , special = Just harbingerSpecial
  }

harbingerChoice :: Choice
harbingerChoice = MoveFromTo
  { n: UpTo one
  , filter: Filter.Any
  , source: Pile.Discard
  , destination: Pile.Deck
  , resolution
  , attack: false
  }

harbingerSpecial :: Special
harbingerSpecial =
  { target: Self
  , command: Choose $ harbingerChoice
  , description: "Look through your discard pile."
    <> " You may put a card from it onto your deck."
  }

baron :: CardSpec
baron = cardWithRequirements c rs
  where
    c :: Card
    c = Card.action
      { name = "Baron"
      , cost = 4
      , buys = one
      , special = Just baronSpecial
      }
    rs :: Array CardSpec
    rs = [estate]

gain4Cash :: Choice
gain4Cash = GainBonus
  { bonus: Cash 4
  , attack: false
  , resolution
  }

gain1Estate :: Choice
gain1Estate = GainCards
  { cardName: "Estate"
  , n: one
  , destination: Pile.Discard
  , attack: false
  , resolution
  }

discard1Estate :: Choice
discard1Estate = MoveFromTo
  { filter: Filter.HasName "Estate"
  , n: Exactly one
  , source: Pile.Hand
  , destination: Pile.Discarding
  , attack: false
  , resolution
  }

discard1EstateAndGain4Cash :: Choice
discard1EstateAndGain4Cash = And
  { choices: [ gain4Cash, discard1Estate ]
  , attack: false
  , resolution
  }

discardOrGain1Estate :: Choice
discardOrGain1Estate = Or
  { choices: [ gain1Estate, discard1EstateAndGain4Cash ]
  , attack: false
  , resolution
  }

baronChoice :: Choice
baronChoice = If
  { condition: HasCard "Estate"
  , choice: discardOrGain1Estate
  , otherwise: Just gain1Estate
  , attack: false
  , resolution
  }

baronSpecial :: Special
baronSpecial =
  { target: Self
  , command: Choose baronChoice
  , description: "You may discard an estate for + $4."
    <> " If you don't, gain an Estate."
  }

goldfish :: CardSpec
goldfish = cardWithRequirements c rs
  where
  c :: Card
  c = Card.action
    { name = "Goldfish"
    , cost = 4
    , buys = one
    , special = Just goldfishSpecial
    }
  rs :: Array CardSpec
  rs = [gold]

goldfishSpecial :: Special
goldfishSpecial =
  { target: Self
  , command: Choose goldfishChoice
  , description: "50% chance to gain a gold to your hand"
  }

goldfishChoice :: Choice
goldfishChoice = let attack = false in If
  { condition: Randomly $ 50 ^. Int._toWire
  , choice: GainCards
    { cardName: "Gold"
    , destination: Pile.Hand
    , n: one
    , resolution
    , attack
    }
  , otherwise: Nothing
  , attack
  , resolution
  }

mountebank :: CardSpec
mountebank = cardWithRequirements c rs
  where
    c :: Card
    c = Card.actionAttack
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
    rs :: Array CardSpec
    rs = [curse, copper]

mountebankChoice :: Choice
mountebankChoice = let attack = true in If
  { condition: HasCard "Curse"
  , choice: Or
    { choices:
      [ MoveFromTo
        { n: Exactly one
        , filter: Filter.HasName "Curse"
        , source: Pile.Hand
        , destination: Pile.Discard
        , attack
        , resolution
        }
      , gainCurseAndCopper
      ]
    , attack
    , resolution
    }
  , attack
  , resolution
  , otherwise: Just gainCurseAndCopper
  }

gainCurseAndCopper :: Choice
gainCurseAndCopper = let attack = true in And
  { choices:
    [ GainCards
      { cardName: "Copper"
      , destination: Pile.Discard
      , n: one
      , attack
      , resolution
      }
    , GainCards
      { cardName: "Curse"
      , destination: Pile.Discard
      , n: one
      , attack
      , resolution
      }
    ]
  , attack
  , resolution
  }

margrave :: CardSpec
margrave = let attack = true in independentCard $ Card.actionAttack
  { name = "Margrave"
  , cost = 5
  , cards = 3
  , buys = one
  , special = Just
    { target: EveryoneElse
    , command: Choose $ And
      { choices:
        [ draw1Card
        , discardDownTo3
        ]
      , attack
      , resolution
      }
    , description: "Each other player draws a card"
      <> ", then discards down to 3 cards in hand."
    }
  }

discardCopper :: Choice
discardCopper = let attack = true in MoveFromTo
  { n: Exactly 1
  , filter: Filter.HasName "Copper"
  , source: Pile.Hand
  , destination: Pile.Discard
  , resolution
  , attack
  }

catpurse :: CardSpec
catpurse = independentCard $ Card.actionAttack
  { name = "Catpurse"
  , cost = 4
  , treasure = 2
  , special = Just
    { target: EveryoneElse
    , command: Choose discardCopper
    , description: "Each other player discards a Copper."
    }
  }

huntingLodge :: CardSpec
huntingLodge = let attack = false in independentCard $ Card.action
  { name = "Hunting Lodge"
  , cost = 5
  , actions = actions 2
  , cards = 1
  , special = Just
    { target: Self
    , command: Choose $ Option
      { choice: And
        { choices:
          [ discardYourHand attack
          , Draw
            { n: 5
            , resolution
            , attack
            }
          ]
          , resolution
          , attack
        }
      , attack
      , resolution
      }
    , description: "You may discard your hand for +5 Cards"
    }
  }

oldWitch :: CardSpec
oldWitch = cardWithRequirements c rs
  where
    attack :: Boolean
    attack = true
    c :: Card
    c = Card.actionAttack
      { name = "Old Witch"
      , cost = 5
      , cards = 3
      , special = Just
        { target: EveryoneElse
        , command: Choose $ And
          { choices:
            [ gainCurse
            , If
              { condition: HasCard "Curse"
              , choice: MoveFromTo
                { n: UpTo one
                , filter: Filter.HasName "Curse"
                , source: Pile.Hand
                , destination: Pile.Trash
                , attack
                , resolution
                }
              , otherwise: Nothing
              , attack
              , resolution
              }
            ]
          , resolution
          , attack
          }
        , description: "Each other player gains a Curse"
          <> " and may trash a Curse from their hand."
        }
      }
    rs :: Array CardSpec
    rs = [curse]

settlers :: CardSpec
settlers = cardWithRequirements c rs
  where
    c :: Card
    c = Card.action
      { name = "Settlers"
      , cost = 2
      , cards = one
      , actions = one
      , special = Just
        { target: Self
        , command: Choose $ MoveFromTo
          { n: UpTo one
          , filter: Filter.HasName "Copper"
          , source: Pile.Discard
          , destination: Pile.Hand
          , resolution
          , attack: false
          }
        , description: "Look through your discard pile."
          <> "You may reveal a Copper from it and put it into your hand."
        }
      }
    rs :: Array CardSpec
    rs = [copper]

junkDealer :: CardSpec
junkDealer = independentCard $ Card.action
  { name = "Junk Dealer"
  , cost = 5
  , cards = one
  , actions = one
  , treasure = one
  , special = Just
    { target: Self
    , command: Choose $ MoveFromTo
      { n: Exactly one
      , filter: Filter.Any
      , source: Pile.Hand
      , destination: Pile.Trash
      , resolution
      , attack: false
      }
    , description: "Trash a card from your hand."
    }
  }

stables :: CardSpec
stables = let attack = false in independentCard $ Card.action
  { name = "Stables"
  , cost = 5
  , special = Just
    { target: Self
    , command: Choose $ Option
      { choice: If
        { condition: HasCardType Treasure
        , otherwise: Nothing
        , choice: And
          { choices:
            [ MoveFromTo
              { filter: Filter.HasType Treasure
              , n: Exactly one
              , source: Pile.Hand
              , destination: Pile.Discarding
              , attack
              , resolution
              }
            , Draw
              { n: 3
              , resolution
              , attack
              }
            , GainActions
              { n: one
              , resolution
              , attack
              }
            ]
            , resolution
            , attack
          }
        , attack
        , resolution
        }
      , attack
      , resolution
      }
    , description: "You may discard a treasure for +3 Cards"
      <> " and +1 Action."
    }
  }

workshop :: CardSpec
workshop = let attack = false in independentCard $ Card.action
  { name = "Workshop"
  , cost = 3
  , special = Just
    { target: Self
    , command: Choose $ GainCard
      { filter: Filter.CostUpTo 4
      , destination: Pile.Discarding
      , attack
      , resolution
      }
    , description: "Gain a card costing up to 4."
    }
  }

artisan :: CardSpec
artisan = let attack = false in independentCard $ Card.action
  { name = "Artisan"
  , cost = 6
  , special = Just
    { target: Self
    , command: Choose $ And
      { choices:
        [ GainCard
          { filter: Filter.CostUpTo 5
          , destination: Pile.Hand
          , attack
          , resolution
          }
        , MoveFromTo
          { n: Exactly one
          , filter: Filter.Any
          , source: Pile.Hand
          , destination: Pile.Deck
          , attack
          , resolution
          }
        ]
      , attack
      , resolution
      }
    , description: "Gain a card to your hand costing up to 5."
      <> "Put a card from your hand onto your deck."
    }
  }

armory :: CardSpec
armory = let attack = false in independentCard $ Card.action
  { name = "Armory"
  , cost = 4
  , special = Just
    { target: Self
    , command: Choose $ GainCard
      { filter: Filter.CostUpTo 4
      , destination: Pile.Deck
      , attack
      , resolution
      }
    , description: "Gain a card onto your deck costing up to 4."
    }
  }

altar :: CardSpec
altar = let attack = false in independentCard $ Card.action
  { name = "Altar"
  , cost = 6
  , special = Just
    { target: Self
    , command: Choose $ And
      { choices:
        [ MoveFromTo
          { n: Exactly one
          , filter: Filter.Any
          , source: Pile.Hand
          , destination: Pile.Trash
          , attack
          , resolution
          }
        , GainCard
          { filter: Filter.CostUpTo 5
          , destination: Pile.Discarding
          , attack
          , resolution
          }
        ]
      , attack
      , resolution
      }
    , description: "Trash a card from your hand."
      <> "Gain a card costing up to 5."
    }
  }

courtyard :: CardSpec
courtyard = independentCard $ Card.action
  { name = "Courtyard"
  , cost = 2
  , cards = 3
  , special = Just
    { target: Self
    , command: Choose $ MoveFromTo
      { n: Exactly one
      , filter: Filter.Any
      , source: Pile.Hand
      , destination: Pile.Deck
      , resolution
      , attack: false
      }
    , description: "Put a card from your hand onto your deck."
    }
  }

lurker :: CardSpec
lurker = let attack = false in independentCard $ Card.action
  { name = "Lurker"
  , cost = 2
  , actions = one
  , special = Just
    { target: Self
    , command: Choose $ Or
      { choices:
        [ GainCard
          { filter: Filter.HasType Action
          , destination: Pile.Trash
          , attack
          , resolution
          }
        , MoveFromTo
          { n: Exactly one
          , filter: Filter.HasType Action
          , source: Pile.Trash
          , destination: Pile.Discarding
          , attack
          , resolution
          }
        ]
      , resolution
      , attack
      }
    , description: "Trash an action card from the supply"
      <> " or gain an action card from the trash."
    }
  }

cellar :: CardSpec
cellar = let
  attack = false
  description = "Discard N cards, then draw N cards."
  in independentCard $ Card.action
  { name = "Cellar"
  , cost = 2
  , actions = one
  , special = Just
    { target: Self
    , command: Choose $ StackChoice
      { expression:
        [ StackChooseCards
          { cards: Unbound
          , filter: Bound Filter.Any
          , from: Bound Pile.Hand
          , n: Bound Unlimited
          }
        , StackDuplicate
        , StackDiscard
        , StackLength
        , StackDraw
        ]
      , stack: []
      , attack
      , description
      }
    , description
    }
  }

remodel :: CardSpec
remodel = let
  attack = false
  description = "Trash a card from your hand."
    <> " Gain a card costing up to 2 more than it."
  in independentCard $ Card.action
  { name = "Remodel"
  , cost = 4
  , special = Just
    { target: Self
    , command: Choose $ StackChoice
      { expression:
        [ StackChooseCards
          { cards: Unbound
          , filter: Bound Filter.Any
          , from: Bound Pile.Hand
          , n: Bound $ Exactly one
          }
        , StackDuplicate
        , StackTrash
        , StackNth zero
        , StackCostOf
        , StackAddN 2
        , StackMakeFilterCostUpTo
        , StackBind "filter"
        , StackChooseCardFromSupply
          { cardName: Unbound
          , filter: Unbound
          }
        , StackGainTo Pile.Discarding
        ]
      , stack: []
      , attack
      , description
      }
    , description
    }
  }

expand :: CardSpec
expand = let
  attack = false
  description = "Trash a card from your hand."
    <> " Gain a card costing up to 3 more than it."
  in independentCard $ Card.action
  { name = "Expand"
  , cost = 7
  , special = Just
    { target: Self
    , command: Choose $ StackChoice
      { expression:
        [ StackChooseCards
          { cards: Unbound
          , filter: Bound Filter.Any
          , from: Bound Pile.Hand
          , n: Bound $ Exactly one
          }
        , StackDuplicate
        , StackTrash
        , StackNth zero
        , StackCostOf
        , StackAddN 3
        , StackMakeFilterCostUpTo
        , StackBind "filter"
        , StackChooseCardFromSupply
          { cardName: Unbound
          , filter: Unbound
          }
        , StackGainTo Pile.Discarding
        ]
      , stack: []
      , attack
      , description
      }
    , description
    }
  }

tradingPost :: CardSpec
tradingPost = cardWithRequirements c rs
    where
      attack :: Boolean
      attack = false
      description :: String
      description = "Trash 2 cards from your hand."
        <> " If you did, gain a Silver to your hand."
      c :: Card
      c = Card.action
        { name = "Trading Post"
        , cost = 5
        , special = Just
          { target: Self
          , command: Choose $ StackChoice
            { expression:
              [ StackChooseCards
                { cards: Unbound
                , filter: Bound Filter.Any
                , from: Bound Pile.Hand
                , n: Bound $ Exactly 2
                }
              , StackDuplicate
              , StackTrash
              , StackLength
              , StackIf
                { condition: [ StackEquals $ StackInt 2 ]
                , following:
                  [ StackPush $ StackString ((_.name <<< Card._card) silver)
                  , StackGainTo Pile.Hand
                  ]
                , otherwise: [ ]
                }
              ]
            , stack: []
            , attack
            , description
            }
          , description
          }
        }
      rs :: Array CardSpec
      rs = [silver]

mine :: CardSpec
mine = let
  attack = false
  description = "You may trash a Treasure card from your hand."
    <> " Gain a Treasure card to your hand"
    <> " costing up to $3 more than it."
  in independentCard $ Card.action
  { name = "Mine"
  , cost = 5
  , special = Just
    { target: Self
    , command: Choose $ StackChoice
      { expression:
        [ StackChooseCards
          { cards: Unbound
          , filter: Bound $ Filter.HasType Treasure
          , from: Bound Pile.Hand
          , n: Bound $ Exactly one
          }
        , StackDuplicate
        , StackTrash
        , StackNth zero
        , StackCostOf
        , StackAddN 3
        , StackMakeFilterCostUpTo
        , StackPush (StackFilter (Filter.HasType Treasure))
        , StackMakeFilterAnd
        , StackBind "filter"
        , StackChooseCardFromSupply
          { cardName: Unbound
          , filter: Unbound
          }
        , StackGainTo Pile.Hand
        ]
      , stack: []
      , attack
      , description
      }
    , description
    }
  }

mill :: CardSpec
mill = let
  attack = false
  description = "You may discard 2 cards, for +$2"
  in independentCard $ Card.actionVictory
  { name = "Mill"
  , cost = 4
  , victoryPoints = one
  , special = Just
    { target: Self
    , command: Choose $ StackChoice
      { expression:
        [ StackChooseCards
          { cards: Unbound
          , filter: Bound Filter.Any
          , from: Bound Pile.Hand
          , n: Bound $ Exactly 2
          }
        , StackDuplicate
        , StackDiscard
        , StackLength
        , StackIf
          { condition: [ StackEquals $ StackInt 2 ]
          , following: [ StackGainBonus $ Cash 2 ]
          , otherwise: []
          }
        ]
      , stack: []
      , attack
      , description
      }
    , description
    }
  }

resolution :: forall a. Maybe a
resolution = Nothing

