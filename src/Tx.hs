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
  , fee
  , sampleTxBS
  ) where
-- }}}


-- IMPORTS
-- {{{
import           Debug.Trace                 (trace)
import           Control.Monad               (replicateM, forM)
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
import qualified Script
import           SECP256K1                   (PubKey, SecKey, SigHash)
import qualified SECP256K1
import qualified Text.Megaparsec             as P
import qualified Text.Megaparsec.Debug       as P
import           TxIn                        (TxIn (..))
import qualified TxIn
import           TxOut                       (TxOut (..))
import qualified TxOut
import           Utils
-- }}}




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
  serialize tx =
    -- {{{
    serializeWithCustomTxIns (Varint.serializeList $ txTxIns tx) tx
    -- }}}
  parser = do
    -- {{{
    txVersion  <- word32ParserLE "version"
    numTxIns   <- Varint.countParser
    txTxIns    <- replicateM numTxIns  parser
    numTxOuts  <- Varint.countParser
    txTxOuts   <- replicateM numTxOuts parser
    txLocktime <- parser
    let txId      = LBS.empty
        txTestnet = False
    return $ Tx {..}
    -- }}}

serializeWithCustomTxIns :: ByteString -> Tx -> ByteString
serializeWithCustomTxIns bs Tx {..} =
  -- {{{
     integralToNBytesLE 4 txVersion
  <> bs
  <> Varint.serializeList txTxOuts
  <> serialize            txLocktime
  -- }}}


fee :: Tx -> IO (Maybe Word)
fee Tx {..} =
  -- {{{
  let
    ioMaybeWords = forM txTxIns (getTxInAmount txTestnet)
    totOut       = foldr (\txOut acc -> txOutAmount txOut + acc) 0 txTxOuts
  in do
  mTotIn <- (sum <$>) . sequence <$> ioMaybeWords
  return $ (\totIn -> totIn - totOut) <$> mTotIn
  -- }}}


verify :: Tx -> IO Bool
verify tx@Tx {..} = do
  -- {{{
  mFee <- fee tx
  case mFee of
    Just fee -> do
      let ioValids :: IO [Bool]
          ioValids = mapM (verifyTxIn tx) txTxIns
      validTxIns <- and <$> ioValids
      return (fee >= 0 && validTxIns)
    Nothing ->
      return False
  -- }}}


--  temporary, for performance v---------v
sigHashForTxIn :: Tx -> TxIn -> Maybe TxOut -> IO (Maybe SigHash)
sigHashForTxIn tx@Tx {..} txIn mTxOutCache =
  -- {{{
  let
    ioMaybeBSs =
      -- {{{
      mapM
        ( \txIn' ->
            if txIn == txIn' then do
              mTxOut <- case mTxOutCache of
                          Just _  ->
                            return mTxOutCache
                          Nothing ->
                            getTxInsTxOut txTestnet txIn
              return $ fmap
                ( \txOut ->
                    TxIn.serializeWithCustomScriptSig
                      (serialize $ txOutScriptPubKey txOut)
                      txIn
                )
                mTxOut
            else
              return $ Just $ TxIn.serializeWithoutScriptSig txIn
        )
        txTxIns
      -- }}}
  in do
  mTxInsBS <- (LBS.concat <$>) . sequence <$> ioMaybeBSs
  return $ do
    txInsBS <- mTxInsBS
    let beforeHash =
             serializeWithCustomTxIns txInsBS tx
          <> integralToNBytesLE 4 1 -- SIGHASH_ALL at the end.
    return $ fromInteger $ bsToInteger $ hash256 beforeHash
  -- }}}


verifyTxIn :: Tx -> TxIn -> IO Bool
verifyTxIn tx@Tx {..} txIn = do
  -- {{{
  -- Returns whether the input has a valid signature
  -- # get the relevant input
  -- # grab the previous ScriptPubKey
  -- # get the signature hash (z)
  -- # combine the current ScriptSig and the previous ScriptPubKey
  -- # evaluate the combined script
  mTxOut   <- getTxInsTxOut txTestnet txIn
  mSigHash <- sigHashForTxIn tx txIn mTxOut
  case (mTxOut, mSigHash) of
    (Just txOut, Just sigHash) ->
      return $ Script.validate
        (txInScriptSig     txIn )
        (txOutScriptPubKey txOut)
        sigHash
    _ ->
      return False
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
    url = fromString $ filter (/= '\"') $ show $ baseEndPoint <> "tx/" <> encodeHex txId <> "/hex"
  in do
  response <- httpLbs url
  return $ P.runParser parser "" $ getResponseBody response
  -- }}}


getTxInsTxOut :: Bool -> TxIn -> IO (Maybe TxOut)
getTxInsTxOut testnet TxIn {..} = do
  -- {{{
  fetchRes <- fetch testnet txInPrevTx
  case fetchRes of
    Right tx ->
      -- {{{
      let
        txOuts    = txTxOuts tx
        outsCount = fromIntegral $ length txOuts
        prevIndex = fromIntegral txInPrevIndex
      in
      if txInPrevIndex < outsCount then
        return $ Just $ txOuts !! prevIndex
      else
        return Nothing
      -- }}}
    Left _ ->
      -- {{{
      return Nothing
      -- }}}
  -- }}}


getTxInAmount :: Bool -> TxIn -> IO (Maybe Word)
getTxInAmount testnet txIn = do
  -- {{{
  mTxOut <- getTxInsTxOut testnet txIn
  return $ fmap txOutAmount mTxOut
  -- }}}


-- SAMPLE VALUE
-- {{{

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


sampleTxBS :: ByteString
sampleTxBS =
     integralToNBytes 4 0x01000000
  <> serialize (Varint 1)
  <> TxIn.sampleTxIn
  <> serialize (Varint 2)
  <> TxOut.sampleTxOut1
  <> TxOut.sampleTxOut2
  <> integralToNBytes 4 0x19430600
-- }}}
