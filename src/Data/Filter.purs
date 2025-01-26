--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defines data type Filter for encoding and decoding card game conditions.
--|
--| ### Key Concepts
--| * **Data Types**: Understanding the `Filter` data type and its variants (`HasName`, `HasType`, `CostUpTo`, `Any`, `And`).
--| * **Generic Programming**: Recognizing the use of generic programming techniques (e.g., `genericDecodeJson`, `genericEncodeJson`) for deriving instances.
--| * **Argonaut Encoding/Decoding**: Understanding how Argonaut is used to encode and decode JSON data, specifically for the `Filter` type.
module Domination.Data.Filter where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Domination.Data.CardType (CardType)

data Filter
  = HasName String
  | HasType CardType
  | CostUpTo Int
  | Any
  | And Filter Filter

derive instance genericFilter :: Generic Filter _
derive instance eqFilter :: Eq Filter
instance showFilter :: Show Filter where
  show condition = genericShow condition
instance encodeJsonFilter :: EncodeJson Filter where
  encodeJson a = genericEncodeJson a
instance decodeJsonFilter :: DecodeJson Filter where
  decodeJson a = genericDecodeJson a
