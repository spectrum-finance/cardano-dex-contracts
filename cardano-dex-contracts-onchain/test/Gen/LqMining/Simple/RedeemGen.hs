{-# LANGUAGE OverloadedStrings #-}

module Gen.LqMining.Simple.RedeemGen where

import Hedgehog

import Gen.Models

import PlutusLedgerApi.V2
import PlutusLedgerApi.V1.Value
import PlutusTx.Builtins.Internal

import qualified PlutusLedgerApi.V1.Interval as PInterval

import qualified ErgoDex.Contracts.Proxy.LqMining.Simple.Redeem as R

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16  as Hex
import qualified Data.Text.Encoding      as E
import qualified Data.Text as T

genRedeemConfig :: AssetClass -> Integer -> PubKeyHash -> R.RedeemConfig
genRedeemConfig = R.RedeemConfig

genRedeemTxIn :: TxOutRef -> OutputDatum -> TxInInfo
genRedeemTxIn txOutRef redeemDatum =
   let
     value = mkValues [mkAdaValue 1000] mempty
     txOut = mkTxOut redeemDatum value mkLMRedeemValidator
   in mkTxIn txOutRef txOut

genUserTxOut :: AssetClass -> Integer -> PubKeyHash -> TxOut
genUserTxOut redeemAC redeemQty userPkh =
   let
     value = mkValues [mkValue redeemAC redeemQty, mkAdaValue 1000] mempty
   in mkTxOut' NoOutputDatum value userPkh

mkLQRedeemTxInfo :: [TxInInfo] -> [TxOut] -> TxInfo
mkLQRedeemTxInfo txIns txOuts =
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
