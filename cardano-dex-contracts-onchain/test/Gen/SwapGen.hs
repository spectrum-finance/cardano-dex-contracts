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

genSConfig :: AssetClass -> AssetClass -> AssetClass -> Integer -> Integer -> Integer -> PubKeyHash -> Integer -> Integer -> (Data, DatumHash)
genSConfig x y nft fee1 fee2 fee3 pkh xQty yQty =
  let 
    config = mkSwapConfig x y nft fee1 fee2 fee3 pkh xQty yQty
    dh     = mkDatumHash $ mkDatum config
  in (toData config, dh)

genSTxIn :: TxOutRef -> DatumHash -> AssetClass -> Integer -> Integer -> TxInInfo
genSTxIn ref dh x xQty adaQty =
  let
    value = mkValues [mkValue x xQty, mkAdaValue adaQty] mempty
    txOut = mkTxOut dh value mkSwapValidator
  in mkTxIn ref txOut

genSTxOut :: DatumHash -> AssetClass -> Integer -> Integer -> PubKeyHash -> TxOut
genSTxOut dh y yQty adaQty pkh =
  let
    value = mkValues [mkValue y yQty, mkAdaValue adaQty] mempty
  in mkTxOut' dh value pkh