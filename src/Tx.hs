{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


-- MODULE
-- {{{
module Tx
  ( Tx
  , getTxId
  , getTxVersion
  , getTxTxIns
  , getTxTxOuts
  , getTxLocktime
  , getTxTestnet
  , makeTx
  , fetch
  ) where
-- }}}


-- IMPORTS
-- {{{
import           Debug.Trace                 (trace)
import           Control.Monad               (replicateM)
import           Data.ByteString.Lazy        (ByteString)
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString             as BS
import           Data.Serializable
import           Data.String                 (fromString)
import           Data.Varint                 (Varint (..))
import qualified Data.Varint                 as Varint
import           Data.Void
import           Data.Word                   (Word32)
import           Extension.ByteString.Parser  
import           Locktime                    (Locktime)
import qualified Locktime
import           Network.HTTP.Simple         (httpLbs, getResponseBody, Request)
import qualified Text.Megaparsec             as P
import qualified Text.Megaparsec.Debug       as P
import           TxIn                        (TxIn (..))
import qualified TxIn
import           TxOut                       (TxOut (..))
import qualified TxOut
import           Utils
-- }}}



-- === Tx =====
-- version:      0x01000000
--
-- txin count:   0x01
-- === TxIn ===
-- prev tx id:   0x813f79011acb80925dfe69b3def355fe914bd1d96a3f5f71bf8303c6a989c7d1
-- prev index:   0x00000000
-- script sig:   0x6b483045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf2132060277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01210349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278a
-- sequence:     0xfeffffff
--
-- txout count:  0x02
-- === TxOut ==
-- amount:       0xa135ef0100000000
-- scriptpubkey: 0x1976a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac
-- txout1        0x99c39800000000001976a9141c4bc762dd5423e332166702cb75f40df79fea1288ac
--
-- locktime:     0x19430600


sampleTx :: ByteString
sampleTx =
     integralToNBytes 4 0x01000000
  <> serialize (Varint 1)
  <> TxIn.sampleTxIn
  <> serialize (Varint 2)
  <> TxOut.sampleTxOut1
  <> TxOut.sampleTxOut2
  <> integralToNBytes 4 0x19430600



data Tx = Tx
  { txId       :: ByteString
  , txVersion  :: Word32
  , txTxIns    :: [TxIn]
  , txTxOuts   :: [TxOut]
  , txLocktime :: Locktime
  , txTestnet  :: Bool
  } deriving (Eq, Show)


getTxId       = txId
getTxVersion  = txVersion
getTxTxIns    = txTxIns
getTxTxOuts   = txTxOuts
getTxLocktime = txLocktime
getTxTestnet  = txTestnet


makeTx :: Word32 -> [TxIn] -> [TxOut] -> Locktime -> Bool -> Tx
makeTx ver ins outs locktime testnet =
  -- {{{
  let
    theTx =
      Tx
        { txId       = LBS.empty
        , txVersion  = ver
        , txTxIns    = ins
        , txTxOuts   = outs
        , txLocktime = locktime
        , txTestnet  = testnet
        }
    ser = serialize theTx
  in
  theTx {txId = encodeHex $ hash256 ser}
  -- }}}


instance Serializable Tx where
  serialize Tx {..} =
    -- {{{
    let
      serializeList serializer theList =
           serialize (Varint $ fromIntegral $ length theList)
        <> foldr (\tx acc -> serializer tx <> acc) LBS.empty theList
    in
       integralToNBytesLE 4 txVersion
    <> serializeList serialize  txTxIns
    <> serializeList serialize txTxOuts
    <> serialize txLocktime
    -- }}}
  parser = do
    -- {{{
    txVersion  <- P.dbg "VERSION" $ word32ParserLE "version"
    numTxIns   <- P.dbg "NUM TXINS" Varint.countParser
    txTxIns    <- P.dbg "TXINS" $ replicateM numTxIns  parser
    numTxOuts  <- P.dbg "NUM TXOUTS" Varint.countParser
    txTxOuts   <- P.dbg "TXOUTS" $ replicateM numTxOuts parser
    txLocktime <- P.dbg "LOCKTIME" parser
    let txId      = LBS.empty
        txTestnet = False
    return $ Tx {..}
    -- }}}


fetch :: Bool -> ByteString -> IO (Either (P.ParseErrorBundle ByteString Void) Tx)
fetch testnet txId =
  -- {{{
  let
    baseEndPoint =
      if testnet then
        "https://blockstream.info/testnet/api/"
      else
        "https://blockstream.info/api/"
    url :: Request
    url = fromString $ show $ baseEndPoint <> "tx/" <> encodeHex txId <> "/hex"
  in do
  response <- httpLbs url
  return $ P.runParser parser "" $ getResponseBody response
  -- }}}
