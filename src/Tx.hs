{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


-- MODULE
-- {{{
module Tx
  ( Tx (..)
  , getTxId
  , fetch
  , fee
  , getTxInsTxOut
  , getTxInAmount
  , sigHashForTxIn
  , verify
  , verifyTxIn
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
import           Data.Char                   (chr)
import           Data.Function               ((&))
import           Data.Functor                (void)
import           Data.Serializable
import           Data.String                 (fromString)
import           Data.Varint                 (Varint (..))
import qualified Data.Varint                 as Varint
import           Data.Void
import           Data.Word                   (Word32)
import           Extension.ByteString.Parser  
import qualified Extension.ByteString.Lazy   as LBS
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
  { txVersion  :: Word32
  , txTxIns    :: [TxIn]
  , txTxOuts   :: [TxOut]
  , txLocktime :: Locktime
  , txTestnet  :: Bool -- ^ Whether the transaction is meant for the testnet.
  } deriving (Eq, Show)


-- | HASH256 of a serialized transaction.
getTxId :: Tx -> ByteString
getTxId tx = hash256 $ serialize tx


instance Serializable Tx where
  serialize tx =
    -- {{{
    serializeWithCustomTxIns (Varint.serializeList $ txTxIns tx) tx
    -- }}}
  parser = do
    -- {{{
    txVersion  <- word32ParserLE "version"
    if txVersion /= 1 then
      void $ P.takeP
        (Just "found the need for this discard of 2 bytes by trial and error")
        2
    else
      return ()
    numTxIns   <- Varint.countParser
    txTxIns    <- replicateM numTxIns  parser
    numTxOuts  <- Varint.countParser
    txTxOuts   <- replicateM numTxOuts parser
    txLocktime <- parser
    let txTestnet = False
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


fee :: Tx -> MaybeT IO Word
fee Tx {..} = do
  -- {{{
  let totOut = foldr (\txOut acc -> txOutAmount txOut + acc) 0 txTxOuts
  words <- forM txTxIns (\txIn -> getTxInAmount txTestnet txIn)
  return $ (sum words) - totOut
  -- }}}


verify :: Tx -> IO Bool
verify tx@Tx {..} = do
  -- {{{
  mFee <- runMaybeT $ fee tx
  case mFee of
    Just fee' -> do
      let ioValids :: IO [Bool]
          ioValids = mapM (verifyTxIn tx) txTxIns
      validTxIns <- and <$> ioValids
      return (fee' >= 0 && validTxIns)
    Nothing ->
      return False
  -- }}}


--   temporary, for performance v---------v
sigHashForTxIn :: Tx -> TxIn -> Maybe TxOut -> MaybeT IO SigHash
sigHashForTxIn tx@Tx {..} txIn mTxOutCache = do
  -- {{{
  txInsBSs <- mapM
                ( \txIn' ->
                    if txIn == txIn' then do
                      txOut <- case mTxOutCache of
                                  Just txOutCache  ->
                                    return txOutCache
                                  Nothing ->
                                    getTxInsTxOut txTestnet txIn
                      return $
                        TxIn.serializeWithCustomScriptSig
                          (serialize $ txOutScriptPubKey txOut)
                          txIn
                    else
                      return $ TxIn.serializeWithoutScriptSig txIn
                )
                txTxIns
  let txInsBS = LBS.concat txInsBSs
      beforeHash =
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
  mRes <- runMaybeT $ do
            txOut <- getTxInsTxOut txTestnet txIn
            sigHash <- sigHashForTxIn tx txIn (Just txOut)
            return $ Script.validate
              (txInScriptSig     txIn )
              (txOutScriptPubKey txOut)
              sigHash
  case mRes of
    Just res ->
      return res
    _ ->
      return False
  -- }}}


fetch :: Bool -> ByteString -> MaybeT IO Tx
fetch testnet txId =
  -- {{{
  let
    baseEndPoint
      | testnet   = "https://blockstream.info/testnet/api/"
      | otherwise = "https://blockstream.info/api/"
    url :: Request
    url =   baseEndPoint <> "tx/" <> encodeHex (LBS.reverse txId) <> "/hex"
          & LBS.unpack
          & map (chr . fromIntegral)
          & fromString
  in do
  response <- liftIO $ httpLbs url
  resBS    <-   getResponseBody response
              & decodeHex
              & eitherToMaybe
              & return
              & MaybeT
  MaybeT                                   $
    return                                 $
    fmap (\tx -> tx {txTestnet = testnet}) $
    eitherToMaybe                          $
    P.runParser parser "" resBS
  -- }}}


getTxInsTxOut :: Bool -> TxIn -> MaybeT IO TxOut
getTxInsTxOut testnet TxIn {..} = do
  -- {{{
  tx <- fetch testnet txInPrevTx
  let txOuts    = txTxOuts tx
      outsCount = fromIntegral $ length txOuts
      prevIndex = fromIntegral txInPrevIndex
  if txInPrevIndex < outsCount then do
    return $ txOuts !! prevIndex
  else do
    fail "invalid txout index."
  -- }}}


getTxInAmount :: Bool -> TxIn -> MaybeT IO Word
getTxInAmount testnet txIn = do
  -- {{{
  txOut <- getTxInsTxOut testnet txIn
  return $ txOutAmount txOut
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
