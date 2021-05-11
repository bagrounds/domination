module Domination.Data.Game.Mode where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Array (head)
import Data.Array.NonEmpty (uncons)
import Data.Maybe (Maybe(..))
import Domination.Data.Game (Game)
import Domination.Data.Player (Player)

data Mode
  = Solo Player
  | Short Player Player
  | Long Player Player

mode
  :: forall m
  . MonadError String m
  => Game
  -> m Mode
mode { players, longGame } =
  case uncons players, longGame of
    { head: player, tail: [] }, _ -> pure $ Solo player
    { head: p1, tail }, true -> Long p1 <$> p2 tail
    { head: p1, tail }, _ -> Short p1 <$> p2 tail
  where
    p2 = head >>> case _ of
      Nothing -> throwError "Where is player 2???"
      Just player2 -> pure player2

