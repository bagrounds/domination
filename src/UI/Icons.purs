{-|
This module provides a collection of icon components used throughout the Domination game UI.
It offers a standardized way to render various game-related icons like actions, buys,
cards, money, points, settings, and more.

The module includes:
- A generic 'icon' function for creating icons with specific CSS classes
- An 'empty' icon for placeholder purposes
- Pre-configured icon components for common game elements
- Consistent styling through CSS class application

Each icon is implemented as a Halogen HTML component, making them easily
composable within the game's interface components.
-}

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

