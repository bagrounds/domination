module Test.Chat (chat_tests) where

import Prelude

import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Array (all, drop, length, mapMaybe, take, (..))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl, sum)
import Data.Lens.Setter ((.~))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.String.CodeUnits (fromCharArray)
import Domination.Capability.Storage (saveFailureMessage)
import Domination.Data.Card (Command(..))
import Domination.Data.Card as Card
import Domination.Data.Constraint (Constraint(..))
import Domination.Data.Filter as Filter
import Domination.Data.Pile as Pile
import Domination.Data.Cards as Cards
import Domination.Data.Chat (Chat, ChatEntry(..), LoggedPlay(..), chatLimits, emptyChat, encodedLength, entries, keepNewestWithin, logPlay, record, recordPlay)
import Domination.Data.Choice (Choice(..))
import Domination.Data.Game (Game)
import Domination.Data.Game as Game
import Domination.Data.Play (Play(..))
import Domination.Data.Player as Player
import Domination.Data.Reaction (Reaction(..))
import Effect (Effect)
import Message (RemoteMessage(..))
import Test.QuickCheck (Result(..), assertEquals, (<?>))
import Test.QuickCheck.Gen (Gen, arrayOf, chooseInt, elements, oneOf, vectorOf)
import Test.Room (all_success, quick_check)
import Data.Array.NonEmpty as NEA

type EffectTest = { name :: String, test :: Effect Result }

chat_tests :: Array EffectTest
chat_tests =
  [ { name: "keepNewestWithin keeps the longest leading run within both limits"
    , test: quick_check 500 do
        maxCount <- chooseInt 0 20
        maxChars <- chooseInt 0 200
        sizes <- arrayOf (chooseInt 0 60)
        let
          kept = keepNewestWithin { maxCount, maxChars } identity sizes
          next = take (length kept + 1) sizes
          isPrefix = kept == take (length kept) sizes
          within = length kept <= maxCount && sum kept <= maxChars
          maximal = length kept == length sizes
            || length next > maxCount
            || sum next > maxChars
        pure $ (isPrefix && within && maximal)
          <?> "limits " <> show { maxCount, maxChars } <> ", sizes " <> show sizes
            <> " kept " <> show kept
    }
  , { name: "for any entries, the saved chat stays within its limits"
    , test: quick_check 40 do
        added <- chooseInt 0 (2 * chatLimits.maxEntries) >>= flip vectorOf entryGen
        let
          chat = foldl (flip record) emptyChat added
          size = encodedLength chat
          count = length (entries chat)
        pure $ (size <= chatLimits.maxChars + chatLimits.maxEntries + 1
            && count <= chatLimits.maxEntries)
          <?> show count <> " entries, " <> show size <> " chars"
    }
  , { name: "a chat filled to its limits still keeps the newest entry"
    , test: quick_check 25 do
        added <- vectorOf (chatLimits.maxEntries + 50) entryGen
        newest <- entryGen
        let chat = foldl (flip record) emptyChat (added <> [ newest ])
        pure $ (map shortened (Array.head $ entries chat) == Just (shortened newest))
          <?> "newest " <> show newest <> " not at the head"
    }
  , { name: "chat text and senders are cut to their limits"
    , test: quick_check 100 do
        text <- longStringGen
        sender <- longStringGen
        let chat = record (Said { sender, text, chatNumber: 1 }) emptyChat
        pure $ case entries chat of
          [ Said said ] ->
            (said.text == String.take chatLimits.maxTextChars text
              && said.sender == String.take chatLimits.maxSenderChars sender)
              <?> "not cut: " <> show (String.length said.text)
          other -> Failed $ "unexpected chat " <> show other
    }
  , { name: "a logged play is small whatever the size of the game"
    , test: quick_check 200 do
        game <- gameGen
        play <- playGen
        let size = encodedLength (logPlay game play)
        pure $ (size <= 4000) <?> show play <> " logged as " <> show size <> " chars"
    }
  , { name: "a chat survives encoding and decoding"
    , test: quick_check 100 do
        added <- arrayOf entryGen
        let chat = foldl (flip record) emptyChat added
        pure $ assertEquals (decodeJson (encodeJson chat) :: Either _ Chat) (Right chat)
    }
  , { name: "logPlay names the card played from the hand"
    , test: pure $ assertEquals
        (logPlay withVillageInHand $ PlayCard { playerIndex: 0, cardIndex: 0 })
        (Just $ PlayedCard { card: "Village" })
    }
  , { name: "logPlay names the card bought from the supply"
    , test: pure $ all_success $ (0 .. 3) <#> \stackIndex -> assertEquals
        (logPlay twoPlayers $ Purchase { playerIndex: 0, stackIndex })
        (Just $ Purchased { card: fromMaybe "???" $ _.card.name <$> Array.index twoPlayers.supply stackIndex })
    }
  , { name: "logPlay names cards a choice moved, and ??? for missing ones"
    , test: pure $ case logPlay withVillageInHand (ResolveChoice { playerIndex: 0, choice: trashFromHand [ 0, 7 ] }) of
        Just (ResolvedChoice { cards }) -> assertEquals cards [ "Village", "???" ]
        other -> Failed $ "unexpected " <> show other
    }
  , { name: "logPlay skips ending a phase and keeps the rest"
    , test: pure $ all_success
        [ assertEquals (logPlay twoPlayers $ EndPhase { playerIndex: 0 }) Nothing
        , assertEquals
            (logPlay twoPlayers $ NewGame { playerCount: 3, supply: Cards.cardMap, longGame: true })
            (Just $ CreatedGame { playerCount: 3 })
        , assertEquals
            (logPlay twoPlayers $ React { playerIndex: 1, reaction: Just BlockAttack })
            (Just $ Reacted { reaction: Just BlockAttack })
        , assertEquals (logPlay twoPlayers $ DoneReacting { playerIndex: 1 }) (Just FinishedReacting)
        ]
    }
  , { name: "a chat saved in the old format is read with plays logged by name"
    , test: pure
        let
          legacy =
            [ ChatMessage { username: "id-1", message: "hi", chatNumber: 3 }
            , PlayMadeMessage { play: PlayCard { playerIndex: 0, cardIndex: 0 }, playerIndex: 0, state: withVillageInHand }
            , PlayMadeMessage { play: EndPhase { playerIndex: 0 }, playerIndex: 0, state: withVillageInHand }
            , JoinMessage { clientId: "id-2", timestamp: 1 }
            ]
        in assertEquals (entries <$> (decodeJson (encodeJson legacy) :: Either _ Chat))
          (Right
            [ Said { sender: "id-1", text: "hi", chatNumber: 3 }
            , Played { playerIndex: 0, play: PlayedCard { card: "Village" } }
            ])
    }
  , { name: "recordPlay adds a logged play and ignores ending a phase"
    , test: pure
        let
          chat = recordPlay { play: EndPhase { playerIndex: 0 }, playerIndex: 0, state: twoPlayers }
            $ recordPlay { play: PlayCard { playerIndex: 0, cardIndex: 0 }, playerIndex: 0, state: withVillageInHand }
            $ emptyChat
        in assertEquals (entries chat) [ Played { playerIndex: 0, play: PlayedCard { card: "Village" } } ]
    }
  , { name: "a failed save's message names the key and size, never the value"
    , test: quick_check 200 do
        key <- shortStringGen
        value <- longStringGen
        browserError <- longStringGen
        let
          message = saveFailureMessage key value browserError
          marked = "VALUE<" <> value <> ">"
        pure $ (String.length message <= String.length key + 300
            && not (String.contains (String.Pattern marked) (saveFailureMessage key marked browserError)))
          <?> "message of " <> show (String.length message) <> " chars for key " <> show key
    }
  ]

shortened :: ChatEntry -> ChatEntry
shortened = case _ of
  Said x -> Said x
    { sender = String.take chatLimits.maxSenderChars x.sender
    , text = String.take chatLimits.maxTextChars x.text
    }
  Notice { text } -> Notice { text: String.take chatLimits.maxTextChars text }
  other -> other

twoPlayers :: Game
twoPlayers = Game.new 2 Cards.cardMap true

withVillageInHand :: Game
withVillageInHand = (Game._player 0 <<< Player._hand .~ [ Card._card Cards.village ]) twoPlayers

trashFromHand :: Array Int -> Choice
trashFromHand indices = MoveFromTo
  { n: UpTo 5
  , filter: Filter.Any
  , source: Pile.Hand
  , destination: Pile.Trash
  , resolution: Just indices
  , attack: false
  }

cardChoices :: Array Choice
cardChoices = mapMaybe (map (_.command >>> commandChoice) <<< _.special) Cards.cardMap
  where
    commandChoice (Choose choice) = choice

entryGen :: Gen ChatEntry
entryGen = oneOf $ NEA.cons'
  (Said <$> ({ sender: _, text: _, chatNumber: _ } <$> longStringGen <*> longStringGen <*> chooseInt 0 1000))
  [ Notice <<< { text: _ } <$> longStringGen
  , mapMaybe' <$> gameGen <*> playGen <*> chooseInt 0 5
  ]
  where
    mapMaybe' game play playerIndex = fromMaybe (Notice { text: "ended a phase" })
      $ Played <<< { playerIndex, play: _ } <$> logPlay game play

gameGen :: Gen Game
gameGen = do
  players <- chooseInt 1 6
  supplySize <- chooseInt 1 (length Cards.cardMap)
  skip <- chooseInt 0 (length Cards.cardMap - supplySize)
  longGame <- elements $ NEA.cons' true [ false ]
  pure $ Game.new players (take supplySize $ drop skip Cards.cardMap) longGame

playGen :: Gen Play
playGen = do
  playerIndex <- chooseInt 0 5
  index <- chooseInt 0 30
  choice <- elements $ NEA.cons' (trashFromHand [ 0, 1, 2, 3, 4 ]) cardChoices
  oneOf $ NEA.cons' (pure $ PlayCard { playerIndex, cardIndex: index })
    [ pure $ Purchase { playerIndex, stackIndex: index }
    , pure $ ResolveChoice { playerIndex, choice }
    , pure $ React { playerIndex, reaction: Just BlockAttack }
    , pure $ NewGame { playerCount: 2, supply: Cards.cardMap, longGame: true }
    , pure $ EndPhase { playerIndex }
    , pure $ DoneReacting { playerIndex }
    ]

shortStringGen :: Gen String
shortStringGen = chooseInt 0 40 >>= stringOfLength

longStringGen :: Gen String
longStringGen = chooseInt 0 1200 >>= stringOfLength

stringOfLength :: Int -> Gen String
stringOfLength n = fromCharArray <$> vectorOf n (elements $ NEA.cons' 'a' [ 'é', '"', '\\', ' ', 'z', '\n' ])
