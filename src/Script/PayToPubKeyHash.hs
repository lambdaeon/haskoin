module Script.PayToPubKeyHash where


import           Crypto.Random           (getRandomBytes)
import qualified Data.ByteString.Lazy as LBS
import           Data.Serializable
import qualified Locktime
import           Script
import           ECC                     (signWith, addressToHash160OfSEC)
import qualified ECC
import           TxIn                    (TxIn (..))
import qualified TxIn
import           TxOut                   (TxOut (..))
import qualified TxOut
import           Tx                      (Tx (..))
import qualified Tx
import           Utils


-- | Takes the HASH160 of an SEC formatted public key point,
--   and returns the `ScriptPubKey` for the p2pkh scheme.
fromHash160 :: ByteString -> ScriptPubKey
fromHash160 bs =
  -- {{{
  Script
    [ OpCommand OP_DUP
    , OpCommand OP_HASH160
    , Element   bs
    , OpCommand OP_EQUALVERIFY
    , OpCommand OP_CHECKSIG
    ]
  -- }}}


-- | Takes a Bitcoin address, and returns the p2pkh `ScriptPubKey`.
fromAddress :: ByteString -> Either Text ScriptPubKey
fromAddress addr58 = do
  -- {{{
  hashOfSEC <- addressToHash160OfSEC addr58
  return $ fromHash160 hashOfSEC
  -- }}}


-- | A function to create a transaction which sends some satoshis
--   from the testnet wallet to another address, and sends back the
--   change to the testnet's change wallet.
testnetPayTo :: ByteString -> Word32 -> Word -> ByteString -> ExceptT Text IO Tx
testnetPayTo prevTx prevIndex targetAmount toAddr = do
  -- {{{
  let fixedFee = 100
      txIn     = TxIn.initWithEmptyScriptSig prevTx prevIndex
  txInsTxOut <- Tx.getTxInsTxOut True txIn
  let changeAmount = txOutAmount txInsTxOut - targetAmount - fixedFee
  if changeAmount < 0 then
    fail "insufficient funds."
  else do
    targetScriptPubKey <- except $ fromAddress toAddr
    changeScriptPubKey <- except $ fromAddress ECC.testnetChangeWallet
    let targetTxOut = TxOut targetAmount targetScriptPubKey
        changeTxOut = TxOut changeAmount changeScriptPubKey
        initTx      = Tx
          { txVersion  = 1
          , txTxIns    = [txIn]
          , txTxOuts   =
              if changeAmount == 0 then
                [ targetTxOut
                ]
              else
                [ targetTxOut
                , changeTxOut
                ]
          , txLocktime = Locktime.make 0
          , txTestnet  = True
          }
    nonce   <-   liftIO
               $ fromInteger . bsToInteger . LBS.fromStrict <$> getRandomBytes 8
    sigHash <- Tx.sigHashForTxIn initTx txIn (Just txInsTxOut)
    signature <- except $ signWith ECC.testnetPrivateKey nonce sigHash
    let der = serialize signature
        sig       = der `LBS.snoc` 0x01
        sec       = ECC.toSEC True ECC.testnetPublicKey
        scriptSig = Script [Script.Element sig, Script.Element sec]
        fnlTxIn   = txIn {txInScriptSig = scriptSig}
        fnlTx     = initTx {txTxIns = [fnlTxIn]}
    return $ initTx {txTxIns = [fnlTxIn]}
  -- }}}








