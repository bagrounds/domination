--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Provides a set of icons for use in a UI application.
--|
--| ### Key Concepts
--| * Halogen HTML generation and CSS class manipulation
--| * Using `icon` function to generate HTML elements with custom CSS classes
--| * Custom HTML elements for various use cases (e.g., actions, buys, cards, etc.)

module Domination.UI.Icons where

import Domination.UI.Css as Css
import Halogen.HTML (ClassName, HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

icon :: forall w i. ClassName -> HTML w i
icon i = HH.i [ HP.classes [ Css.icon, i ] ] []

empty :: forall w i. HTML w i
empty = HH.i [ HP.classes [ Css.icon ] ] []

actions :: forall w i. HTML w i
actions = icon Css.actions

buys :: forall w i. HTML w i
buys = icon Css.buys

cards :: forall w i. HTML w i
cards = icon Css.cards

money :: forall w i. HTML w i
money = icon Css.money

points :: forall w i. HTML w i
points = icon Css.points

settings :: forall w i. HTML w i
settings = icon Css.settings

check :: forall w i. HTML w i
check = icon Css.check
