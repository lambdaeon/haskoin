{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Tx
  ( Tx
  , getTxId
  , getTxVersion
  , getTxTxIns
  , getTxTxOuts
  , getTxLocktime
  , getTxTestnet
  , makeTx
  , parser
  ) where


import           Debug.Trace           (trace)
import           Data.Time.Clock.POSIX (POSIXTime)
import           Data.ByteString.Lazy  (ByteString)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString       as BS
import           Data.Void
import           Data.Word             (Word32)
import           Text.Megaparsec       (Parsec)
import qualified Text.Megaparsec       as P
import qualified Text.Megaparsec.Byte  as BP
import           Script (Script, unScriptSig)
import qualified Script
import           Utils
import           Data.Varint (Varint, unVarint)
import qualified Data.Varint as Varint


data Tx = Tx
  { txId       :: ByteString
  , txVersion  :: Word32
  , txTxIns    :: [TxIn]
  , txTxOuts   :: [TxOut]
  , txLocktime :: POSIXTime
  , txTestnet  :: Bool
  } deriving (Eq, Show)


getTxId       = txId
getTxVersion  = txVersion
getTxTxIns    = txTxIns
getTxTxOuts   = txTxOuts
getTxLocktime = txLocktime
getTxTestnet  = txTestnet



data TxIn = TxIn
  { txInPrevTx    :: ByteString
  , txInPrevIndex :: Word32
  , txInScriptSig :: ScriptSig
  , txInSequence  :: Word32
  } deriving (Eq)
instance Show TxIn where
  -- {{{
  show TxIn{..} = show $
       encodeHex
         ( serialize
             (txVersion  txInPrevTx)
             (txTxIns    txInPrevTx)
             (txTxOuts   txInPrevTx)
             (txLocktime txInPrevTx)
             (txTestnet  txInPrevTx)
         )
    <> ":"
    <> encodeHex (integralToBSLE txInPrevIndex)
  -- }}}


txInParser :: Parser TxIn
txInParser = do
  -- {{{
  let from4Bytes lbl = fromInteger . bsToIntegerLE <$> P.takeP (Just lbl) 4
  txInPrevTx     <- P.takeP (Just "prev tx") 32
  txInPrevIndex  <- from4Bytes "prev index"
  txInScriptSig  <- Script.parser
  txInSequence   <- from4Bytes "sequence"  
  return $ TxIn {..}
  -- }}}


data TxOut = TxOut () deriving (Eq, Show)


makeTx :: Word32 -> [TxIn] -> [TxOut] -> POSIXTime -> Bool -> Tx
makeTx ver ins outs locktime testnet =
  -- {{{
  let
    ser = serialize ver ins outs locktime testnet
  in
  Tx
    { txId       = encodeHex $ hash256 ser
    , txVersion  = ver
    , txTxIns    = ins
    , txTxOuts   = outs
    , txLocktime = locktime
    , txTestnet  = testnet
    }
  -- }}}


serialize :: Word32 -> [TxIn] -> [TxOut] -> POSIXTime -> Bool -> ByteString
serialize ver ins outs locktime testnet = undefined

parser :: Parser Tx
parser = do
  verBytes <- P.takeP (Just "version") 4
  return $ Tx
    { txId       = LBS.empty
    , txVersion  = fromInteger $ bsToIntegerLE verBytes
    , txTxIns    = []
    , txTxOuts   = []
    , txLocktime = 0
    , txTestnet  = False
    }

