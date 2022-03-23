module Data.Serializable where


import           Data.ByteString.Lazy        (ByteString)
import qualified Data.ByteString.Lazy        as LBS
import           Extension.ByteString.Parser  
import           Utils


-- CLASSES
-- {{{
class Serializable a where
  serialize :: a -> ByteString
  parser    :: Parser a
-- }}}
