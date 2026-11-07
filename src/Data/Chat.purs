-- | The room's chat panel, which is also its game log.
-- |
-- | Plays are logged as what was played, with every card index already turned
-- | into a card name, so an entry is a few hundred characters at most instead
-- | of a whole game. The chat is bounded by entry count and by encoded size,
-- | and `record` is the only way to add to it, so every path obeys the bound.
module Domination.Data.Chat
  ( Chat
  , ChatEntry(..)
  , ChatLimits
  , LoggedPlay(..)
  , PlayMade
  , chatLimits
  , emptyChat
  , encodedLength
  , entries
  , fromEntries
  , fromRemoteMessage
  , keepNewestWithin
  , logPlay
  , playedEntry
  , record
  , recordPlay
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Array (filter, length, mapMaybe, scanl, take, takeWhile, (:))
import Data.Generic.Rep (class Generic)
import Data.Lens.Fold ((^?))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Show.Generic (genericShow)
import Data.String as String
import Domination.Data.Card as Card
import Domination.Data.Choice (Choice(..))
import Domination.Data.Game (Game)
import Domination.Data.Game as Game
import Domination.Data.Pile (Pile)
import Domination.Data.Play (Play(..))
import Domination.Data.Player as Player
import Domination.Data.Reaction (Reaction)
import Domination.Data.Stack (_stacksToCardsIso)
import Message (RemoteMessage(..))

type PlayMade = { play :: Play, playerIndex :: Int, state :: Game }

data LoggedPlay
  = CreatedGame { playerCount :: Int }
  | PlayedCard { card :: String }
  | Purchased { card :: String }
  | ResolvedChoice { choice :: Choice, cards :: Array String }
  | Reacted { reaction :: Maybe Reaction }
  | FinishedReacting

derive instance genericLoggedPlay :: Generic LoggedPlay _
derive instance eqLoggedPlay :: Eq LoggedPlay
instance showLoggedPlay :: Show LoggedPlay where
  show = genericShow
instance encodeJsonLoggedPlay :: EncodeJson LoggedPlay where
  encodeJson = genericEncodeJson
instance decodeJsonLoggedPlay :: DecodeJson LoggedPlay where
  decodeJson = genericDecodeJson

data ChatEntry
  = Said { sender :: String, text :: String, chatNumber :: Int }
  | Notice { text :: String }
  | Played { playerIndex :: Int, play :: LoggedPlay }

derive instance genericChatEntry :: Generic ChatEntry _
derive instance eqChatEntry :: Eq ChatEntry
instance showChatEntry :: Show ChatEntry where
  show = genericShow
instance encodeJsonChatEntry :: EncodeJson ChatEntry where
  encodeJson = genericEncodeJson
instance decodeJsonChatEntry :: DecodeJson ChatEntry where
  decodeJson = genericDecodeJson

-- | Newest first, each entry with its encoded size. The constructor is
-- | private: build one with `record` or `fromEntries`, which both keep it
-- | within `chatLimits`.
newtype Chat = Chat (Array Sized)

type Sized = { entry :: ChatEntry, size :: Int }

derive instance eqChat :: Eq Chat
instance showChat :: Show Chat where
  show = entries >>> show >>> ("(Chat " <> _) >>> (_ <> ")")
instance encodeJsonChat :: EncodeJson Chat where
  encodeJson = encodeJson <<< entries

-- | Also reads chats saved before plays were logged by name, which held
-- | `RemoteMessage`s with a whole game in every play.
instance decodeJsonChat :: DecodeJson Chat where
  decodeJson json = fromEntries
    <$> (decodeJson json <|> (mapMaybe fromRemoteMessage <$> decodeJson json))

type ChatLimits =
  { maxEntries :: Int
  , maxChars :: Int
  , maxTextChars :: Int
  , maxSenderChars :: Int
  }

chatLimits :: ChatLimits
chatLimits =
  { maxEntries: 250
  , maxChars: 100000
  , maxTextChars: 500
  , maxSenderChars: 100
  }

emptyChat :: Chat
emptyChat = Chat []

entries :: Chat -> Array ChatEntry
entries (Chat xs) = _.entry <$> xs

record :: ChatEntry -> Chat -> Chat
record entry (Chat xs) = boundChat $ sized entry : xs

recordPlay :: PlayMade -> Chat -> Chat
recordPlay = maybe identity record <<< playedEntry

fromEntries :: Array ChatEntry -> Chat
fromEntries = boundChat <<< map sized

sized :: ChatEntry -> Sized
sized = shortenText >>> \entry -> { entry, size: encodedLength entry }

boundChat :: Array Sized -> Chat
boundChat = Chat
  <<< keepNewestWithin
    { maxCount: chatLimits.maxEntries, maxChars: chatLimits.maxChars }
    _.size
  <<< filter (_.size >>> (_ <= chatLimits.maxChars))

shortenText :: ChatEntry -> ChatEntry
shortenText = case _ of
  Said x -> Said x
    { sender = String.take chatLimits.maxSenderChars x.sender
    , text = String.take chatLimits.maxTextChars x.text
    }
  Notice { text } -> Notice { text: String.take chatLimits.maxTextChars text }
  played -> played

encodedLength :: forall a. EncodeJson a => a -> Int
encodedLength = String.length <<< stringify <<< encodeJson

-- | The longest run of leading elements within both limits.
keepNewestWithin
  :: forall a
  . { maxCount :: Int, maxChars :: Int }
  -> (a -> Int)
  -> Array a
  -> Array a
keepNewestWithin { maxCount, maxChars } size xs = take (min maxCount withinChars) xs
  where
    withinChars = length $ takeWhile (_ <= maxChars) $ scanl (+) 0 (size <$> xs)

playedEntry :: PlayMade -> Maybe ChatEntry
playedEntry { play, playerIndex, state } =
  Played <<< { playerIndex, play: _ } <$> logPlay state play

-- | What a play did, in the game it was played in. `Nothing` for plays the
-- | log doesn't show (ending a phase).
logPlay :: Game -> Play -> Maybe LoggedPlay
logPlay state = case _ of
  NewGame { playerCount } -> Just $ CreatedGame { playerCount }
  EndPhase _ -> Nothing
  PlayCard { playerIndex, cardIndex } ->
    Just $ PlayedCard { card: handCardName playerIndex cardIndex state }
  Purchase { stackIndex } ->
    Just $ Purchased { card: supplyCardName stackIndex state }
  ResolveChoice { playerIndex, choice } ->
    Just $ ResolvedChoice { choice, cards: movedCardNames playerIndex state choice }
  React { reaction } -> Just $ Reacted { reaction }
  DoneReacting _ -> Just FinishedReacting

fromRemoteMessage :: RemoteMessage -> Maybe ChatEntry
fromRemoteMessage = case _ of
  ChatMessage { username, message, chatNumber } ->
    Just $ Said { sender: username, text: message, chatNumber }
  PlayMadeMessage playMade -> playedEntry playMade
  _ -> Nothing

unknownCard :: String
unknownCard = "???"

handCardName :: Int -> Int -> Game -> String
handCardName playerIndex cardIndex state = fromMaybe unknownCard
  $ state ^? Game._player playerIndex <<< Player._cardInHand cardIndex <<< Card._name

supplyCardName :: Int -> Game -> String
supplyCardName stackIndex state = fromMaybe unknownCard
  $ _.card.name <$> state ^? Game._stack stackIndex

pileCardName :: Pile -> Int -> Game -> Int -> String
pileCardName source playerIndex state cardIndex = fromMaybe unknownCard
  $ _.name <$> state ^? Game._pile source playerIndex <<< _stacksToCardsIso <<< ix cardIndex

movedCardNames :: Int -> Game -> Choice -> Array String
movedCardNames playerIndex state = case _ of
  MoveFromTo { source, resolution: Just cardIndices } ->
    pileCardName source playerIndex state <$> cardIndices
  _ -> []
