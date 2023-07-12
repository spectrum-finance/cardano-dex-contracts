module Gen.PoolGen where

import Hedgehog

import Gen.Models
import Gen.DepositGen

import PlutusLedgerApi.V2
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V1.Time

import qualified ErgoDex.Contracts.Pool as P

genPConfig :: AssetClass -> AssetClass -> AssetClass -> AssetClass -> Integer -> [PubKeyHash] -> POSIXTime -> (Data, OutputDatum)
genPConfig x y nft lq fee stakeAdmin swapStartTime =
  let 
    config = mkPoolConfig nft x y lq fee stakeAdmin swapStartTime
    od     = OutputDatum $ mkDatum config
  in (toData config, od)

genPTxIn :: TxOutRef -> OutputDatum -> AssetClass -> Integer -> AssetClass -> Integer -> AssetClass -> Integer -> AssetClass -> Integer -> Integer -> TxInInfo
genPTxIn ref od x xQty y yQty lq lqQty nft nftQty adaQty =
  let
    value = mkValues [mkValue x xQty, mkValue y yQty, mkValue lq lqQty, mkValue nft nftQty, mkAdaValue adaQty] mempty
    txOut = mkTxOut od value mkPoolValidator 
  in mkTxIn ref txOut

genPTxInWithSC :: TxOutRef -> Maybe StakingCredential -> OutputDatum -> AssetClass -> Integer -> AssetClass -> Integer -> AssetClass -> Integer -> AssetClass -> Integer -> Integer -> TxInInfo
genPTxInWithSC ref sc od x xQty y yQty lq lqQty nft nftQty adaQty =
  let
    value = mkValues [mkValue x xQty, mkValue y yQty, mkValue lq lqQty, mkValue nft nftQty, mkAdaValue adaQty] mempty
    txOut = mkTxOutWithSC od value mkPoolValidator sc
  in mkTxIn ref txOut

genPTxOut :: OutputDatum -> AssetClass -> Integer -> AssetClass -> Integer -> AssetClass -> Integer -> AssetClass -> Integer -> Integer -> TxOut
genPTxOut od x xQty y yQty lq lqQty nft nftQty adaQty =
  let
    value = mkValues [mkValue x xQty, mkValue y yQty, mkValue lq lqQty, mkValue nft nftQty, mkAdaValue adaQty] mempty
  in mkTxOut od value mkPoolValidator

genPTxOutWithSC :: OutputDatum -> Maybe StakingCredential -> AssetClass -> Integer -> AssetClass -> Integer -> AssetClass -> Integer -> AssetClass -> Integer -> Integer -> TxOut
genPTxOutWithSC od sc x xQty y yQty lq lqQty nft nftQty adaQty =
  let
    value = mkValues [mkValue x xQty, mkValue y yQty, mkValue lq lqQty, mkValue nft nftQty, mkAdaValue adaQty] mempty
  in mkTxOutWithSC od value mkPoolValidator sc