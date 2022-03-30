module Data.Varint
  ( Varint (..)
  , serializeList
  , countParser
  , lengthPrefixed
  ) where


import           Control.Monad               (unless)
import qualified Data.ByteString.Lazy        as LBS
import           Data.Serializable
import           Extension.ByteString.Parser
import qualified Text.Megaparsec             as P
import qualified Text.Megaparsec.Debug       as P
import           Utils


-- | A newtype wrapper for a `Word32` to represent a varint.
newtype Varint = Varint
  { unVarint :: Word
  } deriving (Eq, Show)

instance Serializable Varint where
  -- | Serialization of a varint into different
  --   number of bytes, depending on the wrapped value:
  --   * If it's less than @0xfd@ (or \(253\)), it'll be encoded in 1 byte.
  --   * If it's less than @0x10000@ (or \(2^16\)), it'll be encoded in 3 bytes.
  --   * If it's less than @0x100000000@ (or \(2^32\)), it'll be encoded in 5 bytes.
  --   * Anything bigger (up to \(2^64 - 1\)) will be encoded in 9 bytes.
  serialize = serializeHelper False
  -- | Can consume up to 9 bytes.
  parser =
    -- {{{
    let
      fromBytes = return . Varint . fromInteger . bsToIntegerLE
    in do
      fstByte <- P.label "Fist byte for a varint" P.anySingle
      if fstByte == 0xfd then do
        -- 0xfd means the next two bytes are the number.
        nextBytes <- P.takeP (Just "A number between 253 and 2^16-1") 2
        fromBytes nextBytes
      else if fstByte == 0xfe then do
        -- 0xfe means the next four bytes are the number.
        nextBytes <- P.takeP (Just "A number between 2^16 and 2^32-1") 4
        fromBytes nextBytes
      else if fstByte == 0xff then do
        -- 0xff means the next eight bytes are the number.
        nextBytes <- P.takeP (Just "A number between 2^32 and 2^64-1") 8
        fromBytes nextBytes
      else
        return $ Varint $ fromIntegral fstByte
    -- }}}


-- | Serialized a list of `Serializable` values, concat them,
--   and prepend their count as a `Varint` for the first byte.
serializeList :: Serializable a => [a] -> ByteString
serializeList [] = LBS.singleton 0
serializeList xs =
     serialize (Varint $ fromIntegral $ length xs) 
  <> foldr ((<>) . serialize) LBS.empty xs


-- | An unexposed helper abstraction to allow `Varint` values
--   to be serialized in either big-endian or little-endian.
serializeHelper :: Bool -> Varint -> ByteString
serializeHelper be (Varint n) =
  -- {{{
  let
    (fromInt, op, fromLD) =
      if be then
        ( integralToBS
        , flip LBS.snoc
        , \toBePadded lenDiff -> LBS.replicate lenDiff 0x00 <> toBePadded
        )
      else
        ( integralToBSLE
        , LBS.cons'
        , \toBePadded lenDiff -> toBePadded <> LBS.replicate lenDiff 0x00
        )
    (beforePadding, targetLength)
      | n < 0xfd        = (fromInt n, 1)
      | n < 0x10000     = (0xfd `op` fromInt n, 3)
      | n < 0x100000000 = (0xfe `op` fromInt n, 5)
      | otherwise       = (0xff `op` fromInt n, 9)
    lengthDiff = targetLength - fromIntegral (LBS.length beforePadding)
  in
  fromLD beforePadding lengthDiff
  -- }}}


-- | Reverse of the `serialize` instance.
serializeBE = serializeHelper True


-- | Parses up to 9 bytes and returns a `Num` value.
countParser :: Num a => Parser a
countParser = fromIntegral . unVarint <$> parser


-- | From: <https://stackoverflow.com/a/70429612>
--
--   Parses a count prefixed (varint format) list of values.
lengthPrefixed :: Show a => Parser a -> Parser [a]
lengthPrefixed elemParser = do
  -- {{{
  bytesCount <- countParser
  start <- P.getOffset
  let finalOffset = start + bytesCount
      checkOffset = do
        new <- P.getOffset
        unless (new < finalOffset) P.empty
  P.many (checkOffset *> elemParser)
  -- }}}
