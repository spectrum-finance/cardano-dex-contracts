{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gen.LqMining.Simple.DepositGen where

import Hedgehog

import Gen.Models

import PlutusLedgerApi.V2
import PlutusLedgerApi.V1.Value
import PlutusTx.Builtins.Internal

import qualified PlutusLedgerApi.V1.Interval as PInterval

import qualified ErgoDex.Contracts.Proxy.LqMining.Simple.Deposit       as D
import qualified ErgoDex.Contracts.Proxy.LqMining.Simple.StakingBundle as SB

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16  as Hex
import qualified Data.Text.Encoding      as E
import qualified Data.Text as T

genRedeemerTxOut :: TxOutRef -> CurrencySymbol -> Integer -> PubKeyHash -> (TxOut, AssetClass)
genRedeemerTxOut TxOutRef{..} bundleCS bundleACQty userPkh =
    let
        bundleAC  = AssetClass (bundleCS, TokenName (getTxId txOutRefId))
        bundleQty = bundleACQty
        value = mkValues [mkValue bundleAC bundleQty, mkAdaValue 1000] mempty
    in (mkTxOut' NoOutputDatum value userPkh, bundleAC)

genDepositTxInInfo :: TxOutRef -> AssetClass -> Integer -> AssetClass -> Integer -> OutputDatum -> TxInInfo
genDepositTxInInfo depositRef vlqAC vlqQty tmpAC tmpQty od =
    let
        value = mkValues [mkValue vlqAC vlqQty, mkValue tmpAC tmpQty, mkAdaValue 1000] mempty
        txOut = mkTxOut od value mkLMDepositValidator
    in mkTxIn depositRef txOut

-- in deposit contract we need only pool TxOutRef
genFakePoolTxInInfo :: TxOutRef -> TxInInfo
genFakePoolTxInInfo poolTxOutRef  =
    let
        value = mkValues [mkAdaValue 1000] mempty
        pkh = PubKeyHash . BuiltinByteString $ "test"
        txOut = mkTxOut' NoOutputDatum value pkh
    in mkTxIn poolTxOutRef txOut

genDepositConfig :: Integer -> CurrencySymbol -> PubKeyHash -> AssetClass -> AssetClass -> D.DepositConfig
genDepositConfig expectedNumEpochs bundleKeyCS redeemerPkh vlqAC tmpAC = 
    D.DepositConfig expectedNumEpochs bundleKeyCS redeemerPkh vlqAC tmpAC

mkLMDepositTxInfo :: [TxInInfo] -> [TxOut] -> TxInfo
mkLMDepositTxInfo txIns txOuts =
   TxInfo
     { txInfoInputs = txIns
     , txInfoOutputs = txOuts
     , txInfoReferenceInputs = mempty
     , txInfoRedeemers = fromList []
     , txInfoFee = mempty
     , txInfoMint = mempty
     , txInfoDCert = []
     , txInfoWdrl = fromList []
     , txInfoValidRange = PInterval.always
     , txInfoSignatories = []
     , txInfoData = fromList []
     , txInfoId = "b0"
     }