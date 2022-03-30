module Script.PayToPubKeyHash where


import           Crypto.Random           (getRandomBytes)
import qualified Data.ByteString.Lazy as LBS
import           Data.Serializable
import qualified Locktime
import           Script
import           SECP256K1               (signWith)
import qualified SECP256K1
import           SECP256K1.S256Point     (addressToHash160OfSEC)
import qualified SECP256K1.S256Point     as S256Point
import           TxIn                    (TxIn (..))
import qualified TxIn
import           TxOut                   (TxOut (..))
import qualified TxOut
import           Tx                      (Tx (..))
import qualified Tx
import           Utils


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


fromAddress :: ByteString -> Maybe ScriptPubKey
fromAddress addr58 = do
  -- {{{
  hashOfSEC <- addressToHash160OfSEC addr58
  return $ fromHash160 hashOfSEC
  -- }}}


testnetPayTo :: ByteString -> Word32 -> Word -> ByteString -> MaybeT IO Tx
testnetPayTo prevTx prevIndex targetAmount toAddr = do
  let txIn = TxIn.initWithEmptyScriptSig prevTx prevIndex
  txInsTxOut <- Tx.getTxInsTxOut True txIn
  let changeAmount = txOutAmount txInsTxOut - targetAmount
  if changeAmount < 0 then
    fail "insufficient funds."
  else do
    targetScriptPubKey <- hoistMaybe $ fromAddress toAddr
    changeScriptPubKey <- hoistMaybe $ fromAddress SECP256K1.testnetChangeWallet
    let targetTxOut = TxOut targetAmount targetScriptPubKey
        changeTxOut = TxOut changeAmount changeScriptPubKey
        initTx      = Tx
          { txVersion  = 1
          , txTxIns    = [txIn]
          , txTxOuts   = [targetTxOut, changeTxOut]
          , txLocktime = Locktime.make 0
          , txTestnet  = True
          }
    nonce   <-   liftIO
               $ (fromInteger . bsToInteger . LBS.fromStrict) <$> getRandomBytes 8
    sigHash <- Tx.sigHashForTxIn initTx txIn (Just txInsTxOut)
    der     <-   hoistMaybe
               $ serialize <$> signWith SECP256K1.testnetPrivateKey nonce sigHash
    let sig       = der `LBS.snoc` 0x01
        sec       = S256Point.toSEC True SECP256K1.testnetPublicKey
        scriptSig = Script [Script.Element sig, Script.Element sec]
        fnlTxIn   = txIn {txInScriptSig = scriptSig}
    return $ initTx {txTxIns = [fnlTxIn]}


