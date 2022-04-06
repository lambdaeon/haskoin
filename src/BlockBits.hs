module BlockBits where


import qualified Data.ByteString.Lazy        as LBS
import           Data.Serializable
import qualified Text.Megaparsec             as P
import           Utils


-- | An explicit datatype to represent the @bits@ field of a block header.
data BlockBits = BlockBits
  { bbExp       :: Word8
  , bbLeftByte  :: Word8
  , bbMidByte   :: Word8
  , bbRightByte :: Word8
  } deriving (Eq, Show)


instance Serializable BlockBits where
  serialize BlockBits {..} =
    -- {{{
    LBS.pack [bbLeftByte, bbMidByte, bbRightByte, bbExp]
    -- }}}
  parser = do
    -- {{{
    bbLeftByte  <- P.anySingle
    bbMidByte   <- P.anySingle
    bbRightByte <- P.anySingle
    bbExp       <- P.anySingle
    return $ BlockBits {..}
    -- }}}


-- | Finds the minig target based on a `BlockBits` value.
toTarget :: BlockBits -> Integer
toTarget BlockBits {..} =
  -- {{{
  let
    coeff = bsToIntegerLE $ LBS.pack [bbLeftByte, bbMidByte, bbRightByte]
    exp   = fromIntegral bbExp
  in
  coeff * 256 ^ (exp - 3)
  -- }}}


-- | A more explicit function (compared to its parser) to get a `BlockBits`
--   from a 4 byte long `ByteString`.
fromByteString :: ByteString -> Maybe BlockBits
fromByteString bs =
  -- {{{
  case LBS.unpack bs of
    [bbLeftByte, bbMidByte, bbRightByte, bbExp] ->
      Just $ BlockBits {..}
    _                                           ->
      Nothing
  -- }}}



