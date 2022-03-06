module Utils where


import           Data.Function           ((&))
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.ByteArray          as BA
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.Binary             as Bin
import qualified Data.Word               as W


convertToBase16 :: ByteString -> ByteString
convertToBase16 = BAE.convertToBase BAE.Base16


integralTo32Bytes :: Integral n => n -> ByteString
integralTo32Bytes = integerTo32Bytes . toInteger


integerTo32Bytes :: Integer -> ByteString
integerTo32Bytes n =
  -- {{{
  Bin.encode n
  & BSL.reverse
  & BSL.toStrict
  & convertToBase16
  & BS.take 64
  -- }}}


prependIntegerWithWord8 :: W.Word8 -> Integer -> ByteString
prependIntegerWithWord8 w8 n =
  -- {{{
  Bin.encode n
  & BSL.reverse
  & BSL.append (Bin.encode w8)
  & BSL.toStrict
  & convertToBase16
  & BS.take 66
  -- }}}


appendWord8ToByteString :: W.Word8 -> ByteString -> ByteString
appendWord8ToByteString w8 bs =
  -- {{{
  Bin.encode w8
  & BSL.toStrict
  & flip BS.append bs
  -- }}}




