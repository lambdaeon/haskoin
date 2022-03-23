{-# LANGUAGE NumericUnderscores #-}


module Locktime
  ( Locktime
  ) where


import           Data.ByteString.Lazy        (ByteString)
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString             as BS
import           Data.Serializable
import           Extension.ByteString.Parser
-- import        Data.Time.Clock.POSIX       (POSIXTime)
import           Data.Word                   (Word32)
import qualified Text.Megaparsec             as P
import           Utils


data Locktime
  = Block Word32
  | POSIX Word32
  deriving (Eq, Show)

instance Serializable Locktime where
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
  parser = do
    -- {{{
    w32 <- word32ParserLE "locktime"
    if w32 < 500_000_000 then
      return $ Block w32
    else
      return $ POSIX w32
    -- }}}
