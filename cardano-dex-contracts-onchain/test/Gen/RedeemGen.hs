module Gen.RedeemGen where

import Hedgehog

import Gen.Models
import Gen.DepositGen

import PlutusLedgerApi.V2
import PlutusLedgerApi.V1.Value

import qualified ErgoDex.Contracts.Proxy.Redeem as R

mkRedeemConfig :: AssetClass -> AssetClass -> AssetClass -> AssetClass -> Integer -> PubKeyHash -> R.RedeemConfig
mkRedeemConfig x y lq nft fee pkh =
  R.RedeemConfig nft x y lq fee pkh Nothing

genRConfig :: AssetClass -> AssetClass -> AssetClass -> AssetClass -> Integer -> PubKeyHash -> (Data, OutputDatum)
genRConfig x y lq nft fee pkh =
  let 
    config = mkRedeemConfig x y lq nft fee pkh
    dh     = OutputDatum $ mkDatum config
  in (toData config, dh)

genRTxOut :: OutputDatum -> AssetClass -> Integer -> AssetClass -> Integer -> Integer -> PubKeyHash -> TxOut
genRTxOut od x xQty y yQty adaQty pkh =
  let
    value = mkValues [mkValue x xQty, mkValue y yQty, mkAdaValue adaQty] mempty
  in mkTxOut' od value pkh

genRTxIn :: TxOutRef -> OutputDatum -> AssetClass -> Integer -> Integer -> TxInInfo
genRTxIn ref od lq lqQty adaQty =
  let
    value = mkValues [mkValue lq lqQty, mkAdaValue adaQty] mempty
    txOut = mkTxOut od value mkSwapValidator
  in mkTxIn ref txOut