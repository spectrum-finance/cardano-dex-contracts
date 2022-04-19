module Gen.PoolGen where

import Hedgehog

import Gen.Models
import Gen.DepositGen

import Plutus.V1.Ledger.Value
import Plutus.V1.Ledger.Api

import qualified ErgoDex.Contracts.Pool as P

genPConfig :: AssetClass -> AssetClass -> AssetClass -> AssetClass -> Integer -> (Data, DatumHash)
genPConfig x y nft lq fee =
  let 
    config = mkPoolConfig nft x y lq fee
    dh     = mkDatumHash $ mkDatum config
  in (toData config, dh)

genPTxIn :: TxOutRef -> DatumHash -> AssetClass -> Integer -> AssetClass -> Integer -> AssetClass -> Integer -> AssetClass -> Integer -> Integer -> TxInInfo
genPTxIn ref dh x xQty y yQty lq lqQty nft nftQty adaQty =
  let
    value = mkValues [mkValue x xQty, mkValue y yQty, mkValue lq lqQty, mkValue nft nftQty, mkAdaValue adaQty] mempty
    txOut = mkTxOut dh value mkPoolValidator
  in mkTxIn ref txOut

genPTxOut :: DatumHash -> AssetClass -> Integer -> AssetClass -> Integer -> AssetClass -> Integer -> AssetClass -> Integer -> Integer -> TxOut
genPTxOut dh x xQty y yQty lq lqQty nft nftQty adaQty =
  let
    value = mkValues [mkValue x xQty, mkValue y yQty, mkValue lq lqQty, mkValue nft nftQty, mkAdaValue adaQty] mempty
  in mkTxOut dh value mkPoolValidator