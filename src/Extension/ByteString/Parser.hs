module Extension.ByteString.Parser where


import           Text.Megaparsec      (Parsec)
import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Byte as BP
import           Utils


-- | Type alias for convenience.
type Parser      = Parsec Void ByteString
-- | Type alias for convenience.
type ParseResult = Either (P.ParseErrorBundle ByteString Void)


-- | Parsing of a `Word32` value from 4 bytes in little-endian.
word32ParserLE :: String -- ^ Label for better parsing messages.
               -> Parser Word32
word32ParserLE lbl = fromInteger . bsToIntegerLE <$> P.takeP (Just lbl) 4


