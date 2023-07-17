{-# LANGUAGE OverloadedStrings #-}

module Gen.Models
  ( genTokenName
  , genTxId
  , genTxOutRef
  , genCurrencySymbol
  , random16bs
  , random28bs
  , random32bs
  , mkAdaAssetClass
  , genAssetClass
  , mkAssetClass
  , mkValue
  , mkAdaValue
  , mkValues
  , mkPoolConfig
  , mkDepositConfig
  , mkPoolRedeemer
  , mkDepositRedeemer
  , mkOrderRedeemer
  , mkRedeemer
  , mkDatum
  , mkDatumHash
  , mkMaxLq
  , mkTxInType
  , mkScriptCredential
  , genPkh
  , mkDepositValidator
  , mkSwapValidator
  , mkPoolValidator
  , mkTxOut
  , mkUserTxOut
  , mkUserTxOutWithDatum
  , mkTxOutWithSC
  , mkTxOut'
  , mkTxIn
  , mkTxInfo
  , mkTxInfoWithSignatures
  , mkTxInfoWithSignaturesAndMinting
  , mkTxInfoOnlyWithSignatures
  , mkPoolTxInfo
  , mkPurpose
  , mkRewardingPurpose
  , mkDelegatingPurpose
  , mkContext
  ) where

import RIO

import Hedgehog
import Hedgehog.Gen   as Gen
import Hedgehog.Range as Range

import qualified Data.ByteString as BS

import PlutusTx.Builtins.Internal
import PlutusLedgerApi.V1.Value 
import qualified PlutusLedgerApi.V1.Value as Value
import PlutusLedgerApi.V2.Tx
import PlutusLedgerApi.V2
import qualified PlutusLedgerApi.V1.Interval as Interval
import Plutarch.Api.V2 ( validatorHash, datumHash)

import qualified ErgoDex.PValidators             as PScripts
import qualified ErgoDex.Contracts.Pool          as P
import qualified ErgoDex.Contracts.Proxy.Deposit as D
import qualified ErgoDex.Contracts.Proxy.Order   as O
import PlutusTx.Builtins as Builtins

genBuiltinByteString :: MonadGen f => Int -> f BuiltinByteString
genBuiltinByteString s = bytes (Range.singleton s) <&> BuiltinByteString

random32bs :: MonadGen f => f BuiltinByteString
random32bs = genBuiltinByteString 32

random28bs :: MonadGen f => f BuiltinByteString
random28bs = genBuiltinByteString 28

random16bs :: MonadGen f => f BuiltinByteString
random16bs = genBuiltinByteString 16

genTxId :: MonadGen f => f TxId
genTxId = prune $ random32bs <&> TxId

genTxOutRef :: MonadGen f => f TxOutRef
genTxOutRef = do
  txId <- genTxId
  ix   <- integral $ Range.constant 0 10
  pure $ TxOutRef txId ix

genTokenName :: MonadGen f => f TokenName
genTokenName = do
  bs <- random32bs
  return $ TokenName bs

genCurrencySymbol :: MonadGen f => f CurrencySymbol
genCurrencySymbol = do
  bs <- random32bs
  return $ CurrencySymbol bs

mkAssetClass :: CurrencySymbol -> TokenName -> AssetClass
mkAssetClass cs tn = AssetClass (cs, tn)

genAssetClass :: MonadGen f => f AssetClass
genAssetClass = do
  tn <- genTokenName
  cs <- genCurrencySymbol
  return $ AssetClass (cs, tn)

mkAdaAssetClass :: AssetClass
mkAdaAssetClass = mkAssetClass adaSymbol adaToken

mkValue :: AssetClass -> Integer -> Value
mkValue (AssetClass (cs, tn)) qty = Value.singleton cs tn qty

mkAdaValue :: Integer -> Value
mkAdaValue qty = mkValue mkAdaAssetClass qty

mkValues :: [Value] -> Value -> Value
mkValues (x:xs) acc = mkValues xs (x <> acc)
mkValues [] acc = acc

mkPoolConfig :: AssetClass -> AssetClass -> AssetClass -> AssetClass -> Integer -> [CurrencySymbol] -> Integer -> P.PoolConfig
mkPoolConfig nft x y lq fee stakeAdminCS lqBound = P.PoolConfig nft x y lq fee stakeAdminCS lqBound

mkDepositConfig :: AssetClass -> AssetClass -> AssetClass -> AssetClass -> Integer -> PubKeyHash -> Integer -> D.DepositConfig
mkDepositConfig nft x y lq fee pkh cFee = D.DepositConfig nft x y lq fee pkh Nothing cFee

mkPoolRedeemer :: Integer -> P.PoolAction -> P.PoolRedeemer
mkPoolRedeemer ix action = P.PoolRedeemer action ix

mkDepositRedeemer :: Integer -> Integer -> Integer -> O.OrderRedeemer
mkDepositRedeemer a b c = O.OrderRedeemer a b c O.Apply

mkOrderRedeemer :: Integer -> Integer -> Integer -> O.OrderRedeemer
mkOrderRedeemer a b c = O.OrderRedeemer a b c O.Apply

mkRedeemer :: ToData a => a -> Redeemer
mkRedeemer = Redeemer . toBuiltinData

mkDatum :: ToData a => a -> Datum
mkDatum = Datum . toBuiltinData

mkDatumHash :: Datum -> DatumHash
mkDatumHash = datumHash

mkMaxLq :: Integer
mkMaxLq = 0x7fffffffffffffff

mkTxInType :: Datum -> Redeemer -> TxInType
mkTxInType datum redeemer = ConsumeScriptAddress PScripts.poolValidator redeemer datum 

mkScriptCredential :: Credential
mkScriptCredential = ScriptCredential $ validatorHash PScripts.poolValidator

genPkh :: MonadGen f => f PubKeyHash
genPkh = genBuiltinByteString 28 <&> PubKeyHash

mkDepositValidator :: ValidatorHash
mkDepositValidator = validatorHash PScripts.depositValidator

mkPoolValidator :: ValidatorHash
mkPoolValidator = validatorHash PScripts.poolValidator

mkSwapValidator :: ValidatorHash
mkSwapValidator = validatorHash PScripts.swapValidator

mkTxOut :: OutputDatum -> Value -> ValidatorHash -> TxOut
mkTxOut od v vh =
  TxOut
    { txOutAddress = Address (ScriptCredential vh) Nothing
    , txOutValue   = v
    , txOutDatum   = od
    , txOutReferenceScript = Nothing
    }

mkUserTxOut :: Value -> PubKeyHash -> TxOut
mkUserTxOut v pkh =
  TxOut
    { txOutAddress = Address (PubKeyCredential pkh) Nothing
    , txOutValue   = v
    , txOutDatum   = NoOutputDatum
    , txOutReferenceScript = Nothing
    }

mkUserTxOutWithDatum :: OutputDatum -> Value -> PubKeyHash -> TxOut
mkUserTxOutWithDatum od v pkh =
  TxOut
    { txOutAddress = Address (PubKeyCredential pkh) Nothing
    , txOutValue   = v
    , txOutDatum   = od
    , txOutReferenceScript = Nothing
    }

mkTxOutWithSC :: OutputDatum -> Value -> ValidatorHash -> Maybe StakingCredential -> TxOut
mkTxOutWithSC od v vh sc =
  TxOut
    { txOutAddress = Address (ScriptCredential vh) sc
    , txOutValue   = v
    , txOutDatum   = od
    , txOutReferenceScript = Nothing
    }

mkTxOut' :: OutputDatum -> Value -> PubKeyHash -> TxOut
mkTxOut' od v pkh =
  TxOut
    { txOutAddress  = Address (PubKeyCredential pkh) Nothing
    , txOutValue    = v
    , txOutDatum    = od
    , txOutReferenceScript = Nothing
    }

mkTxIn :: TxOutRef -> TxOut -> TxInInfo
mkTxIn ref out =
  TxInInfo
    { txInInfoOutRef   = ref
    , txInInfoResolved = out
    }

mkPoolTxInfo :: TxInInfo -> TxOut -> TxInfo
mkPoolTxInfo pIn pOut =
  TxInfo
    { txInfoInputs = [pIn]
    , txInfoOutputs = [pOut]
    , txInfoFee = mempty
    , txInfoMint = mempty
    , txInfoDCert = []
    , txInfoWdrl = fromList []
    , txInfoValidRange = Interval.always
    , txInfoSignatories = mempty
    , txInfoData = fromList []
    , txInfoId = "b0"
    }

mkTxInfo :: TxInInfo -> TxInInfo -> TxOut -> TxOut -> TxInfo
mkTxInfo pIn oIn pOut oOut =
  TxInfo
    { txInfoInputs = [pIn, oIn]
    , txInfoOutputs = [pOut, oOut]
    , txInfoFee = mempty
    , txInfoMint = mempty
    , txInfoDCert = []
    , txInfoWdrl = fromList []
    , txInfoValidRange = Interval.always
    , txInfoSignatories = mempty
    , txInfoData = fromList []
    , txInfoId = "b0"
    }

mkTxInfoWithSignatures :: [TxInInfo] -> [TxOut] -> [PubKeyHash] -> TxInfo
mkTxInfoWithSignatures pIns pOuts sigs =
  TxInfo
    { txInfoInputs = pIns
    , txInfoOutputs = pOuts
    , txInfoFee = mempty
    , txInfoMint = mempty
    , txInfoDCert = []
    , txInfoWdrl = fromList []
    , txInfoValidRange = Interval.always
    , txInfoSignatories = sigs
    , txInfoData = fromList []
    , txInfoId = "b0"
    }

mkTxInfoWithSignaturesAndMinting :: [TxInInfo] -> TxOut -> [PubKeyHash] -> Value -> TxInfo
mkTxInfoWithSignaturesAndMinting pIn pOut sigs mintValue =
  TxInfo
    { txInfoInputs = pIn
    , txInfoOutputs = [pOut]
    , txInfoFee = mempty
    , txInfoMint = mintValue
    , txInfoDCert = []
    , txInfoWdrl = fromList []
    , txInfoValidRange = Interval.always
    , txInfoSignatories = sigs
    , txInfoData = fromList []
    , txInfoId = "b0"
    }

mkTxInfoOnlyWithSignatures :: [PubKeyHash] -> TxInfo
mkTxInfoOnlyWithSignatures sigs =
  TxInfo
    { txInfoInputs = []
    , txInfoOutputs = []
    , txInfoFee = mempty
    , txInfoMint = mempty
    , txInfoDCert = []
    , txInfoWdrl = fromList []
    , txInfoValidRange = Interval.always
    , txInfoSignatories = sigs
    , txInfoData = fromList []
    , txInfoId = "b0"
    }

mkPurpose :: TxOutRef -> ScriptPurpose
mkPurpose = Spending

mkRewardingPurpose :: StakingCredential -> ScriptPurpose
mkRewardingPurpose sc = Rewarding sc

mkDelegatingPurpose :: StakingCredential -> PubKeyHash -> ScriptPurpose
mkDelegatingPurpose sc pkh = Certifying $ DCertDelegDelegate sc pkh

mkContext :: TxInfo -> ScriptPurpose -> ScriptContext
mkContext cxt purpose = ScriptContext cxt purpose