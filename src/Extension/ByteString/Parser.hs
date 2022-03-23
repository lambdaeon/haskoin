module Extension.ByteString.Parser where


import           Debug.Trace (trace)
import           Data.ByteString.Lazy (ByteString)
import           Data.Void
import           Data.Word            (Word8, Word32)
import           Text.Megaparsec      (Parsec)
import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Byte as BP
import           Utils


type Parser  = Parsec Void ByteString


word32ParserLE :: String -> Parser Word32
word32ParserLE lbl = fromInteger . bsToIntegerLE <$> P.takeP (Just lbl) 4


dbg :: Show a => String -> Parser a -> Parser a
dbg lbl p = do
  parsed <- p
  return $ seq (trace (lbl ++ "> " ++ show parsed) parsed) parsed
