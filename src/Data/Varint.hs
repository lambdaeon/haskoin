module Data.Varint
  ( Varint
  , unVarint
  , serialize
  , serializeLE
  , parser
  ) where


import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString      as BS
import           Data.Void
import           Data.Word (Word8, Word)
import           Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Byte as BP
import           Utils


newtype Varint = Varint
  { unVarint :: Word
  } deriving (Eq, Show)


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

serialize   = serializeHelper True
serializeLE = serializeHelper False

parser :: Parser Varint
parser =
  -- {{{
  let
    fromBytes = return . Varint . fromInteger . bsToIntegerLE
  in do
    fstByte <- P.takeP (Just "First byte.") 1
    case LBS.unpack fstByte of
      [0xfd] -> do
        -- 0xfd means the next two bytes are the number.
        nextBytes <- P.takeP (Just "A number between 253 and 2^16-1") 2
        fromBytes nextBytes
      [0xfe] -> do
        -- 0xfe means the next four bytes are the number.
        nextBytes <- P.takeP (Just "A number between 2^16 and 2^32-1") 4
        fromBytes nextBytes
      [0xff] -> do
        -- 0xfe means the next eight bytes are the number.
        nextBytes <- P.takeP (Just "A number between 2^32 and 2^64-1") 8
        fromBytes nextBytes
      _ ->
        fromBytes fstByte
  -- }}}
