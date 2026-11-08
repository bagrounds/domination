module Test.Chat (chat_tests) where

import Prelude

import Control.Monad.Except.Trans (runExceptT)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Array (drop, length, mapMaybe, take, (..))
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..), either)
import Data.Foldable (foldl, sum)
import Data.Lens.Setter ((%~), (.~))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (traverse)
import Domination.Capability.Random (runRandomM)
import Domination.Capability.Storage (saveFailureMessage)
import Domination.Data.AI as AI
import Domination.Data.AI.Strategy as Strategy
import Domination.Data.Bonus (Bonus(..))
import Domination.Data.Card (CardSpec, Command(..))
import Domination.Data.Card as Card
import Domination.Data.CardMovement (CardMovement)
import Domination.Data.Cards as Cards
import Domination.Data.Chat (Chat, ChatEntry(..), LoggedPlay(..), chatLimits, emptyChat, encodedLength, entries, keepNewestWithin, logPlay, record, recordPlay)
import Domination.Data.Choice (Choice(..))
import Domination.Data.Constraint (Constraint(..))
import Domination.Data.Filter as Filter
import Domination.Data.Game (Game)
import Domination.Data.Game as Game
import Domination.Data.Game.Engine as Engine
import Domination.Data.Pile (Pile)
import Domination.Data.Pile as Pile
import Domination.Data.Play (Play(..))
import Domination.Data.Player as Player
import Domination.Data.Reaction (Reaction(..))
import Domination.Data.StackEvaluation (StackExpression(..))
import Domination.Data.Var (Var(..))
import Domination.UI.RenderText (movementText)
import Effect (Effect)
import Message (RemoteMessage(..))
import Test.QuickCheck (Result(..), assertEquals, (<?>))
import Test.QuickCheck.Gen (Gen, arrayOf, chooseInt, elements, oneOf, vectorOf)
import Test.Room (all_success, quick_check)

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
  , { name: "logPlay names the cards a choice moved"
    , test: pure $ assertEquals
        (movedBy villageToTrash $ ResolveChoice { playerIndex: 0, choice: trashFromHand [ 0 ] })
        [ { cards: [ "Village" ], from: Pile.Hand, to: Pile.Trash } ]
    }
  , { name: "logPlay logs a choice the game rejects without card movements"
    , test: pure $ assertEquals
        (movedBy villageToTrash $ ResolveChoice { playerIndex: 0, choice: trashFromHand [ 0, 7 ] })
        []
    }
  , { name: "Remodel, Expand and Mine log what they trashed, then what they gained"
    , test: all_success <$> traverse trashedThenGained
        [ { card: Cards.remodel, trashed: Cards.estate, gained: Cards.silver, to: Pile.Discarding }
        , { card: Cards.expand, trashed: Cards.copper, gained: Cards.silver, to: Pile.Discarding }
        , { card: Cards.mine, trashed: Cards.copper, gained: Cards.silver, to: Pile.Hand }
        ]
    }
  , { name: "Trading Post logs the two cards it trashed and the Silver it gained"
    , test: pure
        let
          game = withChoiceAndHand Cards.tradingPost [ Cards.copper, Cards.estate, Cards.village ]
          play = ResolveChoice { playerIndex: 0, choice: chooseCards [ 0, 1 ] $ specialChoice Cards.tradingPost }
        in assertEquals (movedBy game play)
          [ { cards: [ "Copper", "Estate" ], from: Pile.Hand, to: Pile.Trash }
          , { cards: [ "Silver" ], from: Pile.Supply, to: Pile.Hand }
          ]
    }
  , { name: "card movements read as the game log shows them"
    , test: pure $ all_success
        [ assertEquals (movementText { cards: [ "Copper" ], from: Pile.Hand, to: Pile.Trash }) "trashed: Copper"
        , assertEquals (movementText { cards: [ "Gold" ], from: Pile.Supply, to: Pile.Discarding })
            "gained: Gold to their discard pile"
        , assertEquals (movementText { cards: [ "Silver" ], from: Pile.Supply, to: Pile.Hand })
            "gained: Silver to their hand"
        , assertEquals (movementText { cards: [ "Estate", "Copper" ], from: Pile.Hand, to: Pile.Discard })
            "discarded: Estate, Copper"
        , assertEquals (movementText { cards: [], from: Pile.Hand, to: Pile.Trash }) "trashed: nothing"
        , assertEquals (movementText { cards: [ "Copper" ], from: Pile.Discard, to: Pile.Hand })
            "moved: Copper from their discard pile to their hand"
        , assertEquals (movementText { cards: [ "Village" ], from: Pile.Supply, to: Pile.Trash })
            "trashed from the supply: Village"
        , assertEquals (movementText { cards: [ "Village" ], from: Pile.Trash, to: Pile.Discarding })
            "gained from the trash: Village to their discard pile"
        ]
    }
  , { name: "in bot games, the log names every card that entered or left the trash or the supply"
    , test: all_success <$> traverse (const $ loggedMovesMatchGame 600) (1 .. 3)
    }
  , { name: "a choice logged with the names of the cards it moved is read as card movements"
    , test: pure $ assertEquals
        (decodeJson $ encodeJson
          { tag: "ResolvedChoice"
          , values: [ { choice: trashFromHand [ 0 ], cards: [ "Village" ] } ]
          })
        (Right $ ResolvedChoice
          { choice: trashFromHand [ 0 ]
          , moved: [ { cards: [ "Village" ], from: Pile.Hand, to: Pile.Trash } ]
          })
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

villageToTrash :: Game
villageToTrash = (Game._player 0 <<< Player._choices .~ [ trashFromHand [] ]) withVillageInHand

trashFromHand :: Array Int -> Choice
trashFromHand indices = MoveFromTo
  { n: UpTo 5
  , filter: Filter.Any
  , source: Pile.Hand
  , destination: Pile.Trash
  , resolution: Just indices
  , attack: false
  }

movedBy :: Game -> Play -> Array CardMovement
movedBy game play = case logPlay game play of
  Just (ResolvedChoice { moved }) -> moved
  _ -> []

specialChoice :: CardSpec -> Choice
specialChoice spec = case (Card._card spec).special of
  Just { command: Choose choice } -> choice
  Nothing -> GainBonus { bonus: Cash 0, attack: false, resolution: Nothing }

withChoiceAndHand :: CardSpec -> Array CardSpec -> Game
withChoiceAndHand card hand = twoPlayers # Game._player 0 %~
  ( (Player._hand .~ (Card._card <$> hand))
  >>> (Player._choices .~ [ specialChoice card ])
  )

chooseCards :: Array Int -> Choice -> Choice
chooseCards cardIndices = bindFirstStep case _ of
  StackChooseCards x -> StackChooseCards x { cards = Bound cardIndices }
  other -> other

chooseFromSupply :: String -> Choice -> Choice
chooseFromSupply cardName = bindFirstStep case _ of
  StackChooseCardFromSupply x -> StackChooseCardFromSupply x { cardName = Bound cardName }
  other -> other

bindFirstStep :: (StackExpression -> StackExpression) -> Choice -> Choice
bindFirstStep bind = case _ of
  StackChoice x -> StackChoice x
    { expression = fromMaybe x.expression $ Array.modifyAt 0 bind x.expression }
  other -> other

trashedThenGained
  :: { card :: CardSpec, trashed :: CardSpec, gained :: CardSpec, to :: Pile }
  -> Effect Result
trashedThenGained { card, trashed, gained, to } = do
  let
    game = withChoiceAndHand card [ trashed, Cards.village ]
    trash = ResolveChoice { playerIndex: 0, choice: chooseCards [ 0 ] $ specialChoice card }
    gainedName = (Card._card gained).name
  eAfterTrash <- runRandomM $ runExceptT $ Engine.makePlay trash game
  pure case eAfterTrash of
    Left e -> Failed $ (Card._card card).name <> ": " <> e
    Right afterTrash ->
      let
        gain = ResolveChoice
          { playerIndex: 0
          , choice: chooseFromSupply gainedName
            $ fromMaybe (specialChoice card)
            $ Array.head (NEA.head afterTrash.players).choices
          }
      in all_success
        [ assertEquals (movedBy game trash)
            [ { cards: [ (Card._card trashed).name ], from: Pile.Hand, to: Pile.Trash } ]
        , assertEquals (movedBy afterTrash gain)
            [ { cards: [ gainedName ], from: Pile.Supply, to } ]
        ]

-- | Plays bots for up to `steps` plays, checking each play's log against the
-- | change it made to the trash and the supply, the piles everyone sees.
loggedMovesMatchGame :: Int -> Effect Result
loggedMovesMatchGame steps = runRandomM do
  eGame <- Engine.makeAutoPlay
    (NewGame { playerCount: 2, supply: Cards.cardMap, longGame: false })
    twoPlayers
  either (pure <<< Failed) (go steps) eGame
  where
    bots = [ { playerIndex: 0, strategy: Strategy.Random }, { playerIndex: 1, strategy: Strategy.Random } ]
    go remaining game = case AI.findBotToAct bots game of
      Just bot | remaining > 0 -> do
        ePlay <- AI.generateBotPlay bot game
        case ePlay of
          Left e -> pure $ Failed e
          Right play -> Engine.makeAutoPlay play game >>= case _ of
            Left e -> pure $ Failed e
            Right game'
              | publicChange game game' == loggedChange (logPlay game play) ->
                  go (remaining - 1) game'
              | otherwise -> pure $ Failed $ show play
                  <> " changed the trash and supply by " <> show (publicChange game game')
                  <> " but was logged as " <> show (logPlay game play)
      _ -> pure Success

type PublicChange =
  { intoTrash :: Array String
  , outOfTrash :: Array String
  , intoSupply :: Array String
  , outOfSupply :: Array String
  }

publicChange :: Game -> Game -> PublicChange
publicChange before after =
  { intoTrash: Array.sort $ trashNames after `Array.difference` trashNames before
  , outOfTrash: Array.sort $ trashNames before `Array.difference` trashNames after
  , intoSupply: Array.sort $ supplyNames after `Array.difference` supplyNames before
  , outOfSupply: Array.sort $ supplyNames before `Array.difference` supplyNames after
  }
  where
    trashNames = map _.name <<< _.trash
    supplyNames = Array.concatMap (\{ card, count } -> Array.replicate count card.name) <<< _.supply

loggedChange :: Maybe LoggedPlay -> PublicChange
loggedChange logged =
  { intoTrash: cardsWhere (_.to >>> eq Pile.Trash)
  , outOfTrash: cardsWhere (_.from >>> eq Pile.Trash)
  , intoSupply: cardsWhere (_.to >>> eq Pile.Supply)
  , outOfSupply: cardsWhere (_.from >>> eq Pile.Supply)
  }
  where
    cardsWhere moving = Array.sort $ Array.concatMap _.cards $ Array.filter moving movements
    movements = case logged of
      Just (ResolvedChoice { moved }) -> moved
      Just (Purchased { card }) -> [ { cards: [ card ], from: Pile.Supply, to: Pile.Buying } ]
      _ -> []

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
