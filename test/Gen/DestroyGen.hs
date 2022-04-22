{-# LANGUAGE OverloadedStrings #-}

module Gen.DestroyGen where

import Hedgehog

import Gen.Models
import Gen.DepositGen

import qualified Plutus.V1.Ledger.Interval as Interval
import Plutus.V1.Ledger.Value
import Plutus.V1.Ledger.Api

import qualified ErgoDex.Contracts.Pool as P

genDTxIn :: TxOutRef -> DatumHash -> AssetClass -> Integer -> AssetClass -> Integer -> Integer -> TxInInfo
genDTxIn ref dh lq lqQty nft nftQty adaQty =
  let
    value = mkValues [mkValue lq lqQty, mkValue nft nftQty, mkAdaValue adaQty] mempty
    txOut = mkTxOut dh value mkPoolValidator
  in mkTxIn ref txOut

mkDTxInfo :: TxInInfo -> TxInfo
mkDTxInfo pIn =
  TxInfo
    { txInfoInputs = [pIn]
    , txInfoOutputs = []
    , txInfoFee = mempty
    , txInfoMint = mempty
    , txInfoDCert = []
    , txInfoWdrl = []
    , txInfoValidRange = Interval.always
    , txInfoSignatories = mempty
    , txInfoData = []
    , txInfoId = "b0"
    }