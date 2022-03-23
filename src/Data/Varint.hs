module Data.Varint
  ( Varint (..)
  , countParser
  , lengthPrefixed
  ) where


import           Debug.Trace                 (trace)
import           Control.Monad               (unless)
import           Data.ByteString.Lazy        (ByteString)
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString             as BS
import           Data.Serializable
import           Data.Void
import           Data.Word                   (Word)
import           Extension.ByteString.Parser
import           Text.Megaparsec             (Parsec)
import qualified Text.Megaparsec             as P
import qualified Text.Megaparsec.Debug       as P
import qualified Text.Megaparsec.Byte        as BP
import           Utils


newtype Varint = Varint
  { unVarint :: Word
  } deriving (Eq, Show)

instance Serializable Varint where
  serialize = serializeHelper False
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


serializeBE = serializeHelper True


countParser :: Num a => Parser a
countParser = fromIntegral . unVarint <$> parser


-- from: https://stackoverflow.com/a/70429612
lengthPrefixed :: Show a => Parser a -> Parser [a]
lengthPrefixed elemParser' = do
  -- {{{
  let elemParser = P.dbg "ELEM PARSER" elemParser'
  bytesCount <- countParser
  start <- P.getOffset
  let finalOffset = (trace ("OFFSET START: " ++ show start) start) + bytesCount
      checkOffset = do
        new <- P.getOffset
        unless ((trace ("NEW OFFSET: " ++ show new) new) < (trace ("FINAL OFFSET: " ++ show finalOffset) finalOffset)) P.empty
  P.many (checkOffset *> elemParser)
  -- }}}
