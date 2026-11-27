-- | Settings → Storage: everything this site keeps in the browser, grouped by
-- | owner, with Show, Copy and Delete for each entry and a Clear for each
-- | group. Every delete asks first, in place.
module Domination.UI.Storage
  ( StorageView
  , confirmation
  , context
  , render
  ) where

import Prelude

import AppAction (AppAction(..))
import AppState (StoragePanel, StorageSnapshot)
import Data.Array (filter, find, null)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import DebugLog (showVersion)
import Domination.Data.Room (Room(..), unRoomCode)
import Domination.Data.StorageInventory (Area(..), Context, Group(..), Item, Locator(..), Measure(..), Plan, Removal(..), Section, Selection(..), Summary(..), areaName, areaOf, describeItem, discoveredEntries, emptyAreas, formatCount, formatSize, groupLabel, inventory, isDeletable, item, locatorName, measureOf, removalPlan, roughShare, summaryOf, unavailableAreas)
import Domination.UI.Css as Css
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Version (version)

-- | `unsentMove`: this room has a move its mailbox hasn't confirmed yet.
type StorageView =
  { panel :: StoragePanel
  , room :: Room
  , unsentMove :: Boolean
  }

context :: Room -> String -> Context
context room scope = { room, version: showVersion version, scope }

render :: forall w. StorageView -> HTML w AppAction
render view = HH.div
  [ HP.class_ Css.storagePanel ]
  $ [ HH.div
      [ HP.class_ Css.storagePanelHeader ]
      [ HH.button
        [ HE.onClick \_ -> RefreshStorage ]
        [ HH.text "Refresh" ]
      ]
    ]
  <> maybe [ HH.p_ [ HH.text "Looking at what this site stores…" ] ] (renderSnapshot view) view.panel.snapshot

renderSnapshot :: forall w. StorageView -> StorageSnapshot -> Array (HTML w AppAction)
renderSnapshot view snapshot@{ discovery } =
  [ HH.p_ [ HH.text $ discovery.origin <> " · " <> persistence ]
  , HH.p_ [ HH.text $ localStorageLine sections ]
  ]
  <> maybe [] (\{ usage, quota } -> [ HH.p_ [ HH.text $ estimateLine usage quota ] ]) discovery.estimate
  <> (unavailableLine <$> unavailableAreas discovery)
  <> (renderSection view snapshot items <$> sections)
  <> emptyLine (emptyAreas discovery)
  <> [ renderClearAll view snapshot items ]
  where
    ctx = context view.room snapshot.scope
    items = item ctx <$> discoveredEntries discovery
    sections = inventory ctx (discoveredEntries discovery)
    persistence = case discovery.estimate of
      Just { persisted: true } -> "Kept until you clear it"
      Just _ -> "Kept until the browser needs space (not persistent)"
      Nothing -> "The browser doesn't say how long it keeps this data"
    unavailableLine (Tuple area reason) = HH.p
      [ HP.class_ Css.storageNote ]
      [ HH.text $ "Couldn't read " <> areaName area <> ": " <> reason ]
    emptyLine areas
      | null areas = []
      | otherwise = [ HH.p [ HP.class_ Css.storageNote ] [ HH.text $ "Empty: " <> joinWith ", " (areaName <$> areas) ] ]

localStorageLine :: Array Section -> String
localStorageLine sections = "Local storage: " <> formatSize Characters (toNumber size)
  <> " in " <> formatCount entries <> " keys (" <> roughShare usualLimit (toNumber size)
  <> " of the usual limit of about 5 million characters)"
  where
    Summary { size, entries } = summaryOf
      $ filter ((_ == LocalStorage) <<< areaOf <<< _.locator)
      $ sections >>= _.items
    usualLimit = 5242880.0

estimateLine :: Number -> Number -> String
estimateLine usage quota = "Browser estimate: " <> formatSize Bytes usage <> " used of "
  <> formatSize Bytes quota <> " allowed (all storage types)"

groupMeasure :: Group -> Measure
groupMeasure = case _ of
  OtherArea area -> measureOf area
  AppFiles -> Bytes
  _ -> Characters

summaryText :: Measure -> Summary -> String
summaryText measure (Summary { entries, size, unknownSizes }) =
  formatSize measure (toNumber size) <> " · " <> countText entries
    <> if unknownSizes > 0 then " (" <> formatCount unknownSizes <> " of unknown size)" else ""
  where
    countText = case measure of
      Characters -> \n -> formatCount n <> if n == 1 then " key" else " keys"
      Bytes -> \n -> formatCount n <> if n == 1 then " entry" else " entries"

sectionLabel :: Room -> Group -> String
sectionLabel room = case _ of
  ThisRoom -> case room of
    GlobalRoom -> "Global room (this room)"
    GameRoom code -> "Room " <> unRoomCode code <> " (this room)"
  group -> groupLabel group

renderSection :: forall w. StorageView -> StorageSnapshot -> Array Item -> Section -> HTML w AppAction
renderSection view snapshot items { group, summary, items: sectionItems } = HH.details
  [ HP.class_ Css.storageGroup ]
  $ [ HH.summary_
      [ HH.text $ sectionLabel view.room group <> " · " <> summaryText (groupMeasure group) summary ]
    ]
  <> groupAction
  <> (renderItem view snapshot items <$> sectionItems)
  where
    selection = if group == AppFiles then ReinstallAppFiles else WholeGroup group
    plan = removalPlan (context view.room snapshot.scope) items selection
    groupAction
      | null plan.items = []
      | otherwise =
          [ HH.div
            [ HP.class_ Css.storageActions ]
            [ askButton view selection (not (group == AppFiles && not snapshot.online)) $ groupActionLabel group ]
          ]
          <> renderConfirmation view snapshot.discovery.origin selection plan
    groupActionLabel = case _ of
      AppFiles -> if snapshot.online then "Reinstall" else "Reinstall (needs a connection)"
      OtherRoom _ -> "Clear room"
      ThisRoom -> "Clear room"
      _ -> "Clear"

renderItem :: forall w. StorageView -> StorageSnapshot -> Array Item -> Item -> HTML w AppAction
renderItem view snapshot items i@{ locator, size } = HH.div
  [ HP.class_ Css.storageItem ]
  $ [ HH.div
      [ HP.class_ Css.storageItemRow ]
      $ [ HH.span [ HP.class_ Css.storageItemLabel ] [ HH.text label ]
        , HH.span [ HP.class_ Css.storageItemName ] [ HH.text $ if name == label then "" else name ]
        , HH.span [ HP.class_ Css.storageItemSize ] [ HH.text $ maybe "size unknown" (formatSize measure <<< toNumber) size ]
        ]
      <> valueButtons
      <> (if isDeletable i then [ askButton view selection true "Delete" ] else [])
    ]
  <> renderConfirmation view snapshot.discovery.origin selection plan
  <> shownValue
  <> outcome
  where
    { label } = describeItem i
    name = locatorName locator
    measure = measureOf $ areaOf locator
    selection = OneItem locator
    plan = removalPlan (context view.room snapshot.scope) items selection
    valueButtons = case locator of
      WebStorageKey _ _ ->
        [ case view.panel.shown of
            Just shown | shown.locator == locator -> HH.button [ HE.onClick \_ -> HideStoredValue ] [ HH.text "Hide" ]
            _ -> HH.button [ HE.onClick \_ -> ShowStoredValue locator ] [ HH.text "Show" ]
        , HH.button [ HE.onClick \_ -> CopyStoredValue locator ] [ HH.text "Copy" ]
        ]
      _ -> []
    shownValue = case view.panel.shown of
      Just shown | shown.locator == locator ->
        [ HH.textarea
          [ HP.class_ Css.storageValue
          , HP.readOnly true
          , HP.value shown.text
          , HP.attr (HH.AttrName "aria-label") $ "Value of " <> name
          ]
        ]
      _ -> []
    outcome = case find (\(Tuple l _) -> l == locator) view.panel.outcomes of
      Just (Tuple _ (Waiting reason)) -> [ note $ "Waiting: " <> reason ]
      Just (Tuple _ (Failed reason)) -> [ note $ "Couldn't delete: " <> reason ]
      _ -> []
    note text = HH.p [ HP.class_ Css.storageNote ] [ HH.text text ]

renderClearAll :: forall w. StorageView -> StorageSnapshot -> Array Item -> HTML w AppAction
renderClearAll view snapshot items = HH.div
  [ HP.class_ Css.storageClearAll ]
  $ (if null plan.items then [] else [ askButton view AllDomination true "Clear all Domination data" ])
  <> renderConfirmation view snapshot.discovery.origin AllDomination plan
  where
    plan = removalPlan (context view.room snapshot.scope) items AllDomination

askButton :: forall w. StorageView -> Selection -> Boolean -> String -> HTML w AppAction
askButton view selection enabled text = HH.button
  [ HE.onClick \_ -> AskToRemove selection
  , HP.disabled $ not enabled || view.panel.confirming == Just selection
  ]
  [ HH.text text ]

renderConfirmation :: forall w. StorageView -> String -> Selection -> Plan -> Array (HTML w AppAction)
renderConfirmation view origin selection plan
  | view.panel.confirming /= Just selection = []
  | otherwise =
      [ HH.div
        [ HP.class_ Css.storageConfirm
        , HP.attr (HH.AttrName "role") "alert"
        ]
        $ (HH.p_ <<< pure <<< HH.text <$> confirmation { origin, unsentMove: view.unsentMove } selection plan)
        <> [ HH.button [ HE.onClick \_ -> ConfirmRemoval selection ] [ HH.text $ confirmLabel selection ]
           , HH.button [ HE.onClick \_ -> CancelRemoval ] [ HH.text "Cancel" ]
           ]
      ]
  where
    confirmLabel = case _ of
      ReinstallAppFiles -> "Reinstall"
      _ -> "Delete"

-- | The question a removal asks before it happens: what goes, how much, and
-- | what follows.
confirmation :: { origin :: String, unsentMove :: Boolean } -> Selection -> Plan -> Array String
confirmation { origin, unsentMove } selection plan =
  [ question ]
  <> listed
  <> consequence
  <> (if plan.reloadsThisPage then [ "This page will reload." ] else [])
  <> (if plan.touchesRoomSync && unsentMove then [ "Your last move hasn't reached the room's mailbox yet; it will be lost." ] else [])
  <> sharedOrigin
  where
    Summary { entries, size } = summaryOf plan.items
    measure = maybe Characters (measureOf <<< areaOf <<< _.locator) $ find (const true) plan.items
    question = case selection of
      ReinstallAppFiles -> "Reinstall the app files? They're downloaded again when the page reloads."
      _ -> "Delete " <> formatCount entries <> (if entries == 1 then " entry" else " entries")
        <> " (" <> formatSize measure (toNumber size) <> ")?"
    listed = case selection of
      OneItem _ -> []
      _ -> [ joinWith ", " $ locatorName <<< _.locator <$> plan.items ]
    consequence = case selection, plan.items of
      OneItem _, [ only ] -> [ (describeItem only).consequence ]
      _, _ -> []
    sharedOrigin = case selection of
      OneItem _ -> []
      _ | origin /= "https://domination.fun" -> [ "Other apps on " <> origin <> " may use some of the same names." ]
        | otherwise -> []
