module BlockBits where


import           Data.Bits
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


-- | Finds the mining target based on a `BlockBits` value.
toTarget :: BlockBits -> Integer
toTarget BlockBits {..} =
  -- {{{
  let
    coeff = bsToIntegerLE $ LBS.pack [bbLeftByte, bbMidByte, bbRightByte]
    exp   = fromIntegral bbExp
  in
  coeff * 256 ^ (exp - 3)
  -- }}}


-- | Converts an integer back to a `BlockBits` value.
fromTarget :: Integer -> Either Text BlockBits
fromTarget n =
  -- {{{
  let
    bs = integerToBS n
  in
  case LBS.unpack bs of
    b0 : b1 : rest | b0 > 0x7f ->
      -- {{{
      Right $ BlockBits
        { bbExp       = fromIntegral (LBS.length bs) + 1
        , bbLeftByte  = b1
        , bbMidByte   = b0
        , bbRightByte = 0x00
        }
      -- }}}
    b0 : b1 : b2 : rest ->
      -- {{{
      Right $ BlockBits
        { bbExp       = fromIntegral (LBS.length bs)
        , bbLeftByte  = b2
        , bbMidByte   = b1
        , bbRightByte = b0
        }
      -- }}}
    _                        ->
      -- {{{
      Left "invalid target."
      -- }}}
  -- }}}


-- | A more explicit function (compared to its parser) to get a `BlockBits`
--   from a 4 byte long `ByteString`.
fromByteString :: ByteString -> Either Text BlockBits
fromByteString bs =
  -- {{{
  case LBS.unpack bs of
    [bbLeftByte, bbMidByte, bbRightByte, bbExp] ->
      Right $ BlockBits {..}
    _                                           ->
      Left "invalid bytestring for bits."
  -- }}}



