{-# LANGUAGE OverloadedStrings #-}

module Gen.LqMining.Simple.StakingBundleGen where

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

genStakingBundleTxOut :: AssetClass -> Integer -> AssetClass -> Integer -> OutputDatum -> TxOut
genStakingBundleTxOut vlqAC vlqQty tmpAC tmpQty od =
    let
        value = mkValues [mkValue vlqAC vlqQty, mkValue tmpAC tmpQty, mkAdaValue 1000] mempty
    in mkTxOut od value mkLMStakingBundleValidator

genStakingBundleConfig :: AssetClass -> AssetClass -> AssetClass -> AssetClass -> AssetClass -> PubKeyHash -> SB.StakingBundleConfig
genStakingBundleConfig bundleAC poolAC bundleLQAC bundleVLQAC bundleTMPAC pkh = 
    SB.StakingBundleConfig bundleAC poolAC bundleLQAC bundleVLQAC bundleTMPAC pkh

genUserTxOut :: AssetClass -> Integer -> PubKeyHash -> TxOut
genUserTxOut redeemAC redeemQty userPkh =
   let
     value = mkValues [mkValue redeemAC redeemQty, mkAdaValue 1000] mempty
   in mkTxOut' NoOutputDatum value userPkh

mkLMStakingBundleTxInfo :: [TxInInfo] -> [TxOut] -> TxInfo
mkLMStakingBundleTxInfo inputs outputs =
   TxInfo
     { txInfoInputs = inputs
     , txInfoOutputs = outputs
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
