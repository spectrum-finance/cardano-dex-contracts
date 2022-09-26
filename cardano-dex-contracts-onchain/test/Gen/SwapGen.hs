module Gen.SwapGen where

import Hedgehog

import Gen.Models
import Gen.DepositGen

import PlutusLedgerApi.V2
import PlutusLedgerApi.V1.Value

import qualified ErgoDex.Contracts.Proxy.Swap as S

mkSwapConfig :: AssetClass -> AssetClass -> AssetClass -> Integer -> Integer -> Integer -> PubKeyHash -> Integer -> Integer -> S.SwapConfig
mkSwapConfig x y nft fee1 fee2 fee3 pkh xQty yQty =
  S.SwapConfig x y nft fee1 fee2 fee3 pkh Nothing xQty yQty

genSConfig :: AssetClass -> AssetClass -> AssetClass -> Integer -> Integer -> Integer -> PubKeyHash -> Integer -> Integer -> (Data, OutputDatum)
genSConfig x y nft fee1 fee2 fee3 pkh xQty yQty =
  let 
    config = mkSwapConfig x y nft fee1 fee2 fee3 pkh xQty yQty
    dh     = OutputDatum $ mkDatum config
  in (toData config, dh)

genSTxIn :: TxOutRef -> OutputDatum -> AssetClass -> Integer -> Integer -> TxInInfo
genSTxIn ref od x xQty adaQty =
  let
    value = mkValues [mkValue x xQty, mkAdaValue adaQty] mempty
    txOut = mkTxOut od value mkSwapValidator
  in mkTxIn ref txOut

genSTxOut :: OutputDatum -> AssetClass -> Integer -> Integer -> PubKeyHash -> TxOut
genSTxOut od y yQty adaQty pkh =
  let
    value = mkValues [mkValue y yQty, mkAdaValue adaQty] mempty
  in mkTxOut' od value pkh