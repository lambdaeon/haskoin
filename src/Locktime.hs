module Locktime
  ( Locktime
  , make
  ) where


import           Data.Serializable
import           Extension.ByteString.Parser
-- import        Data.Time.Clock.POSIX       (POSIXTime)
import qualified Text.Megaparsec             as P
import           Utils


-- | Sum type for explicit representation of time.
data Locktime
  = Block Word32 -- ^ Opaque constructor meant for values less than 500 million.
  | POSIX Word32 -- ^ Opaque constructor meant for the rest of the values.
  deriving (Eq, Show)

instance Serializable Locktime where
  -- | Little-endian serialization of the wrapped 
  --   unsigned number in 4 bytes.
  serialize lt =
    -- {{{
    let
      w32 =
        case lt of
          Block w -> w
          POSIX w -> w
    in
    integralToNBytesLE 4 w32
    -- }}}
  -- | Consumes 4 bytes and utilizes the `make` smart constructor.
  parser = do
    -- {{{
    w32 <- word32ParserLE "locktime"
    return $ make w32
    -- }}}

-- | Exposed smart constructor for a `Locktime` value.
make :: Word32 -> Locktime
make w32
  -- {{{
  | w32 < 500_000_000 = Block w32
  | otherwise         = POSIX w32
  -- }}}
