module Gen.RedeemGen where

import Hedgehog

import Gen.Models
import Gen.DepositGen

import Plutus.V1.Ledger.Value
import Plutus.V1.Ledger.Api

import qualified ErgoDex.Contracts.Proxy.Redeem as R

mkRedeemConfig :: AssetClass -> AssetClass -> AssetClass -> AssetClass -> Integer -> PubKeyHash -> R.RedeemConfig
mkRedeemConfig x y lq nft fee pkh =
  R.RedeemConfig nft x y lq fee pkh

genRConfig :: AssetClass -> AssetClass -> AssetClass -> AssetClass -> Integer -> PubKeyHash -> (Data, DatumHash)
genRConfig x y lq nft fee pkh =
  let 
    config = mkRedeemConfig x y lq nft fee pkh
    dh     = mkDatumHash $ mkDatum config
  in (toData config, dh)