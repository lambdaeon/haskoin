module Script
  ( ScriptSig
  , unScriptSig
  , parser
  ) where


import           Data.ByteString.Lazy (ByteString)
import           Data.Varint          (Varint, unVarint)
import qualified Data.Varint          as Varint
import           Data.Void
import           Text.Megaparsec       (Parsec)
import qualified Text.Megaparsec       as P
import qualified Text.Megaparsec.Byte  as BP
import           Utils


newtype ScriptSig = ScriptSig {unScriptSig :: ByteString} deriving (Eq)


parser :: Parser ScriptSig
parser = do
  -- {{{
  scriptSigLen <- fromIntegral . unVarint <$> Varint.parser
  ScriptSig <$> P.takeP (Just "scriptsig" ) scriptSigLen
  -- }}}
