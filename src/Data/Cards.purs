module Domination.Data.Cards where

import Prelude

import Data.Array (findIndex, (!!))
import Data.Lens.Iso (Iso', iso)
import Data.Maybe (Maybe(..), fromMaybe)
import Domination.Data.Bonus (Bonus(..))
import Domination.Data.Card (Card, Command(..))
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

_toWire :: Iso' Card Int
_toWire = iso to from where
  to card = fromMaybe (-1) $ findIndex (_ == card) cardMap
  from i = fromMaybe Card.card $ cardMap !! i

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
  , harem
  , nobles
  ]

copper :: Card
copper = Card.treasure { name = "Copper", treasure = 1 }
silver :: Card
silver = Card.treasure { name = "Silver", cost = 3, treasure = 2 }
gold :: Card
gold = Card.treasure { name = "Gold", cost = 6, victoryPoints = 0, treasure = 3 }
platinum :: Card
platinum = Card.treasure { name = "Platinum", cost = 9, treasure = 5 }
estate :: Card
estate = Card.victory { name = "Estate", cost = 2, victoryPoints  = 1 }
duchy :: Card
duchy = Card.victory { name = "Duchy", cost = 5, victoryPoints = 3 }
province :: Card
province = Card.victory { name = "Province", cost = 8, victoryPoints = 6 }
colony :: Card
colony = Card.victory { name = "Colony", cost = 11, victoryPoints = 10 }
curse :: Card
curse = Card.card { types = [Curse], name = "Curse", victoryPoints = -1 }
greatHall :: Card
greatHall = Card.victory { types = [Action, Victory], name = "Great Hall", cost = 3, cards = 1, actions = 1, victoryPoints = 1 }
village :: Card
village = Card.action { name = "Village", cost = 3, cards = 1, actions = 2 }
woodCutter :: Card
woodCutter = Card.action { name = "Wood Cutter", cost = 3, buys = 1, treasure = 2 }
laboratory :: Card
laboratory = Card.action { name = "Laboratory", cost = 5, cards = 2, actions = 1 }
smithy :: Card
smithy = Card.action { name = "Smithy", cost = 4, cards = 3 }
festival :: Card
festival = Card.action { name = "Festival", cost = 5, actions = 2, buys = 1, treasure = 2 }
market :: Card
market = Card.action { name = "Market", cost = 5, actions = 1, cards = 1, buys = 1, treasure = 1 }
harem :: Card
harem = Card.treasure { types = [Treasure, Victory], name = "Harem", cost = 6, treasure = 2, victoryPoints = 2 }
bazaar :: Card
bazaar = Card.action { name = "Bazaar", cost = 5, cards = 1, actions = 2, treasure = 1 }
monument :: Card
monument = Card.action { types = [Action, Victory], name = "Monument", cost = 4, treasure = 2, victoryPoints = 1 }
workersVillage :: Card
workersVillage = Card.action { name = "Worker's Village", cost = 4, cards = 1, actions = 2, buys = 1 }
witch :: Card
witch = let attack = true in
  Card.actionAttack
  { name = "Witch"
  , cost = 5
  , cards = 2
  , specials =
    [ { target: EveryoneElse
      , command: Choose $ GainCards
        { cardName: "Curse"
        , n: 1
        , resolution: Nothing
        , attack
        }
      , description: "Each other player gains a Curse."
      }
    ]
  }
councilRoom :: Card
councilRoom = let attack = false in
  Card.action
  { name = "Council Room"
  , cost = 5
  , cards = 4
  , buys = 1
  , specials =
    [ { target: EveryoneElse
      , command: Choose $ Draw
        { n: 1
        , resolution: Nothing
        , attack
        }
      , description: "Each other player draws a card."
      }
    ]
  }
scholar :: Card
scholar = let attack = false in
  Card.action
  { name = "Scholar"
  , cost = 5
  , specials =
    [ { target: Self
      , command: Choose $ Discard
        { selection: SelectAll
        , resolution: Nothing
        , attack
        }
      , description: "Discard your hand."
      }
    , { target: Self
      , command: Choose $ Draw
        { n: 7
        , resolution: Nothing
        , attack
        }
      , description: "Draw 7 cards"
      }
    ]
  }
chapel :: Card
chapel = let attack = false in
  Card.action
  { name = "Chapel"
  , cost = 2
  , specials =
    [ { target: Self
      , command: Choose
        $ MoveFromTo
        { n: UpTo 4
        , filter: Nothing
        , source: Pile.Hand
        , destination: Pile.Trash
        , resolution: Nothing
        , attack
        }
      , description: "Trash up to 4 cards from your hand"
      }
    ]
  }
militia :: Card
militia = let attack = true in
  Card.actionAttack
  { name = "Militia"
  , cost = 4
  , treasure = 2
  , specials =
    [ { target: EveryoneElse
      , command: Choose
        $ MoveFromTo
        { n: DownTo 3
        , filter: Nothing
        , source: Pile.Hand
        , destination: Pile.Discard
        , resolution: Nothing
        , attack
        }
      , description: "Each other player discards down to 3 cards"
      }
    ]
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
  , specials =
    [ { target: Self
      , command: Choose $ Or
        { choices:
          [ Draw { n: 3, attack, resolution: Nothing }
          , GainActions { n: 2, attack, resolution: Nothing }
          ]
        , resolution: Nothing
        , attack
        }
      , description: "Choose one: +3 cards or +2 actions"
      }
    ]
  }
steward :: Card
steward = let attack = false in
  Card.action
  { name = "Steward"
  , cost = 3
  , specials =
    [ { target: Self
      , command: Choose $ Or
        { choices:
          [ Draw { n: 2, attack, resolution: Nothing }
          , GainBonus { bonus: Cash 2, attack, resolution: Nothing }
          , MoveFromTo
            { source: Pile.Hand
            , destination: Pile.Trash
            , filter: Nothing
            , n: Exactly 2
            , attack
            , resolution: Nothing
            }
          ]
        , resolution: Nothing
        , attack
        }
      , description:
        "Choose one: + 2 cards, + $2, or trash 2 cards from your hand"
      }
    ]
  }
pawn :: Card
pawn = let attack = false in
  Card.action
  { name = "Pawn"
  , cost = 2
  , specials =
    [ { target: Self
      , command: Choose $ PickN
        { n: 2
        , choices:
          [ Draw { n: 1, attack, resolution: Nothing }
          , GainBonus { bonus: Cash 1, attack, resolution: Nothing }
          , GainActions { n: 1, attack, resolution: Nothing }
          , GainBuys { n: 1, attack, resolution: Nothing }
          ]
        , resolution: Nothing
        , attack
        }
      , description:
        "Choose two of: + $1, + 1 card, + 1 action, or +1 buy"
      }
    ]
  }
torturer :: Card
torturer = let attack = true in
  Card.actionAttack
  { name = "Torturer"
  , cost = 5
  , cards = 3
  , specials =
    [ { target: EveryoneElse
      , command: Choose $ PickN
        { n: 1
        , choices:
          [ MoveFromTo
            { n: Exactly 2
            , filter: Nothing
            , source: Pile.Hand
            , destination: Pile.Discard
            , attack
            , resolution: Nothing
            }
          , GainCards
            { n: 1
            , cardName: "Curse"
            , attack
            , resolution: Nothing
            }
          ]
        , resolution: Nothing
        , attack
        }
      , description:
        "Each other player either discards 2 cards or gains a Curse to their hand, their choice. (They may pick an option they can't do.)"
      }
    ]
  }
consolation :: Card
consolation = let attack = false in
  Card.action
  { name = "Consolation"
  , cost = 2
  , specials =
    [ { target: Self
      , command: Choose $ If
        { condition: HasCard "Estate"
        , choice: GainBonus
          { bonus: Cash 2
          , attack
          , resolution: Nothing
          }
        , otherwise: Nothing
        , resolution: Nothing
        , attack
        }
      , description:
        "If you have an Estate in your hand, + $2"
      }
    ]
  }

moneyLender :: Card
moneyLender = let attack = false in
  Card.action
  { name = "Money Lender"
  , cost = 4
  , specials =
    [ { target: Self
      , command: Choose $ If
        { condition: HasCard "Copper"
        , choice: Option
          { choice: And
            { choices:
              [ MoveFromTo
                { n: Exactly 1
                , filter: Just (HasName "Copper")
                , source: Pile.Hand
                , destination: Pile.Trash
                , attack
                , resolution: Nothing
                }
              , GainBonus
                { bonus: Cash 3
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
      , description:
        "You may trash a copper from your hand for + $3"
      }
    ]
  }

harbinger :: Card
harbinger = let attack = false in
  Card.action
  { name = "Harbinger"
  , cost = 3
  , cards = 1
  , actions = 1
  , specials =
    [ { target: Self
      , command: Choose $ MoveFromTo
        { n: UpTo 1
        , filter: Nothing
        , source: Pile.Discard
        , destination: Pile.Deck
        , resolution: Nothing
        , attack
        }
      , description:
        "Look through your discard pile. You may put a card from it onto your deck."
      }
    ]
  }

baron :: Card
baron = let
  attack = false
  plus4 = GainBonus
    { bonus: Cash 4
    , attack
    , resolution: Nothing
    }
  discardEstate = MoveFromTo
    { filter: Just $ HasName "Estate"
    , n: Exactly 1
    , source: Pile.Hand
    , destination: Pile.Discard
    , attack
    , resolution: Nothing
    }
  gainEstate = GainCards
    { cardName: "Estate"
    , n: 1
    , attack
    , resolution: Nothing
    }
  discardEstateAndPlus4 = And
    { choices: [ plus4, discardEstate ]
    , attack
    , resolution: Nothing
    }
  discardOrGainEstate = Or
    { choices: [ discardEstateAndPlus4, gainEstate ]
    , attack
    , resolution: Nothing
    }
  command = Choose $ If
    { condition: HasCard "Estate"
    , choice: discardOrGainEstate
    , otherwise: Just gainEstate
    , attack
    , resolution: Nothing
    }
  in
  Card.action
  { name = "Baron"
  , cost = 4
  , buys = 1
  , specials =
    [ { target: Self
      , command
      , description: "You may discard an estate for + $4."
        <> " If you don't, gain an Estate."
      }
    ]
  }

