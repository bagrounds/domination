module Domination.UI.Card
  ( render
  ) where

import Prelude
import Data.Maybe (Maybe)
import Domination.Data.Card (Card)
import Domination.Data.Card as Card
import Domination.Data.CardType as CardType
import Domination.UI.Css as Css
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.MouseEvent (MouseEvent)

render
  :: forall a b
  . (MouseEvent -> Maybe b)
  -> Array H.ClassName
  -> Card
  -> HTML a b
render onClick extraClasses card = HH.div
  ( if Card.isTreasure card
    then [ HP.class_ Css.treasureCard ]
    else [ HP.class_ Css.noTreasureCard ]
  )
  [ HH.div (if Card.isVictory card then [ HP.class_ Css.victoryCard ] else [ HP.class_ Css.noVictoryCard ])
    [ HH.div
      ( if Card.hasType CardType.Attack card
        then [ HP.class_ Css.attackCard ]
        else if Card.hasType CardType.Reaction card
        then [ HP.class_ Css.reactionCard ]
        else if Card.hasType CardType.Action card
        then [ HP.class_ Css.actionCard ]
        else if Card.hasType CardType.Curse card
        then [ HP.class_ Css.curseCard ]
        else [ HP.class_ Css.noActionCard ]
      )
      [ HH.button
        [ HE.onClick onClick
        , HP.classes $ [ Css.card ] <> extraClasses
        ]
        [ HH.ul_
          [ HH.li
            [ HP.classes [ Css.cardText, Css.cardName ] ]
            [ HH.text $ " " <> card.name ]
          , HH.li
            [ HP.classes [ Css.cardText, Css.cardCards ] ]
            [ HH.text (if card.cards > 0 then " +" <> show card.cards <> " Card" else "") ]
          , HH.li
            [ HP.classes [ Css.cardText, Css.cardActions ] ]
            [ HH.text $ (if card.actions > 0 then " +" <> show card.actions <> " Action" else "") ]
          , HH.li
            [ HP.classes [ Css.cardText, Css.cardBuys ] ]
            [ HH.text (if card.buys > 0 then " +" <> show card.buys <> " Buy" else "") ]
          , HH.li
            [ HP.classes [ Css.cardText, Css.cardTreasure ] ]
            [ HH.text (if card.treasure > 0 then " +$" <> show card.treasure else "") ]
          , HH.li
            [ HP.classes [ Css.cardText, Css.cardVictoryPoints ] ]
            [ HH.text
              ( if card.victoryPoints > 0
                then " +" <> show card.victoryPoints <> " VP"
                else if card.victoryPoints < 0
                  then show card.victoryPoints
                  else ""
              )
            ]
          , HH.li
            [ HP.classes [ Css.cardText, Css.cardCost ] ]
            [ HH.text $ "Cost $" <> show card.cost ]
          ]
        ]
      ]
    ]
  ]

