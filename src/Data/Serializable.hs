module Data.Serializable where


import qualified Data.ByteString.Lazy        as LBS
import           Extension.ByteString.Parser  
import           Utils


-- CLASSES
-- {{{
-- | Class to define serialization/deserialization scheme
--   to/from a lazy `ByteString`.
class Serializable a where
  serialize :: a -> ByteString
  parser    :: Parser a
-- }}}
