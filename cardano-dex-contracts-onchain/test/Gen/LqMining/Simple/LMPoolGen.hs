{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gen.LqMining.Simple.LMPoolGen where

import Hedgehog

import Gen.Models

import PlutusLedgerApi.V2
import PlutusLedgerApi.V1.Value
import PlutusTx.Builtins.Internal

import qualified PlutusLedgerApi.V1.Interval as PInterval

import qualified ErgoDex.Contracts.Proxy.LqMining.Simple.LMPool as SLMP

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16  as Hex
import qualified Data.Text.Encoding      as E
import qualified Data.Text as T

genLMPoolConfig 
  :: Integer 
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> AssetClass
  -> AssetClass
  -> AssetClass
  -> AssetClass
  -> AssetClass
  -> SLMP.LMPoolConfig
genLMPoolConfig epochLen epochNum programStart programBudget execBudget epoch poolNft poolX poolLQ poolVLQ poolTMP = 
    SLMP.LMPoolConfig epochLen epochNum programStart programBudget execBudget epoch 10 poolNft poolX poolLQ poolVLQ poolTMP

genPoolTxInInfo :: TxOutRef -> Integer -> Integer -> Integer -> Integer -> Integer -> SLMP.LMPoolConfig -> TxInInfo
genPoolTxInInfo poolRef poolNftQty poolXQty poolLQQty poolVLQQty poolTMPQty cfg =
    let txOut = genPoolTxOut poolNftQty poolXQty poolLQQty poolVLQQty poolTMPQty cfg
     in mkTxIn poolRef txOut

genPoolTxOut :: Integer -> Integer -> Integer -> Integer -> Integer -> SLMP.LMPoolConfig -> TxOut
genPoolTxOut poolNftQty poolXQty poolLQQty poolVLQQty poolTMPQty cfg@SLMP.LMPoolConfig{..} =
    let
        value = 
            mkValues 
                [ mkValue poolNft  poolNftQty
                , mkValue poolX    poolXQty
                , mkValue poolLQ   poolLQQty
                , mkValue poolVLQ  poolVLQQty
                , mkValue poolTMP  poolTMPQty
                , mkAdaValue 1000
                ] mempty

        poolDatum = OutputDatum $ mkDatum cfg
    in mkTxOut poolDatum value mkLMPoolValidator

mkLMPoolTxInfo :: [TxInInfo] -> [TxOut] -> Integer -> Integer -> TxInfo
mkLMPoolTxInfo txIns txOuts validRangeStart validRangeEnd =
   let 
     lower = (POSIXTime validRangeStart)
     upper = (POSIXTime validRangeEnd)
   in TxInfo
     { txInfoInputs = txIns
     , txInfoOutputs = txOuts
     , txInfoReferenceInputs = mempty
     , txInfoRedeemers = fromList []
     , txInfoFee = mempty
     , txInfoMint = mempty
     , txInfoDCert = []
     , txInfoWdrl = fromList []
     , txInfoValidRange = PInterval.interval lower upper
     , txInfoSignatories = []
     , txInfoData = fromList []
     , txInfoId = "b0"
     }