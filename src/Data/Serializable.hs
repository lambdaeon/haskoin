module Data.Serializable where


import qualified Data.ByteString.Lazy        as LBS
import           Extension.ByteString.Parser  
import qualified Text.Megaparsec             as P
import           Utils


-- CLASSES
-- {{{
-- | Class to define serialization/deserialization scheme
--   to/from a lazy `ByteString`.
class Serializable a where
  serialize :: a -> ByteString
  parser    :: Parser a
  --
  parse     :: ByteString -> ParseResult a
  parse     = P.runParser parser ""
-- }}}


-- INSTANCES
-- {{{
instance Serializable Bool where
  serialize b
    -- {{{
    | b         = LBS.singleton 0x01
    | otherwise = LBS.singleton 0x00
    -- }}}
  parser = (/= 0x00) <$> P.anySingle

instance Serializable Word8 where
  serialize = LBS.singleton 
  parser = P.anySingle

instance Serializable Word16 where
  serialize w16 = integralToNBytesLE 2 w16
  parser = fromIntegral . bsToIntegerLE <$> P.takeP (Just "two bytes.") 2

instance Serializable Word32 where
  serialize w32 = integralToNBytesLE 4 w32
  parser = fromIntegral . bsToIntegerLE <$> P.takeP (Just "four bytes.") 4

instance Serializable Word64 where
  serialize w64 = integralToNBytesLE 8 w64
  parser = fromIntegral . bsToIntegerLE <$> P.takeP (Just "eight bytes.") 8
-- }}}
