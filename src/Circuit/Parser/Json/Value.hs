-- | The JSON tree.
--
-- One type, six constructors, no policy. Duplicate object keys are preserved
-- in source order; what to do about them is a consumer decision, not a
-- parsing one.
module Circuit.Parser.Json.Value
  ( Json (..),
  )
where

import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Vector (Vector)

-- | A JSON value.
--
-- 'JNumber' is exact ('Scientific': coefficient times ten to an exponent),
-- so parsing loses nothing. 'JObject' is an association list in source
-- order.
data Json
  = JNull
  | JBool !Bool
  | JNumber !Scientific
  | JString !Text
  | JArray (Vector Json)
  | JObject [(Text, Json)]
  deriving (Eq, Show)
