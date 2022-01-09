{-# LANGUAGE NoImplicitPrelude #-}

module ErgoDex.Contracts.Erased.Coins where

import Ledger.Value   (AssetClass, Value, assetClassValueOf)
import PlutusTx.Prelude
import ErgoDex.Plutus (adaAssetClass)

{-# INLINABLE isUnit #-}
isUnit :: Value -> AssetClass -> Bool
isUnit v c = assetClassValueOf v c == 1

{-# INLINABLE isAda #-}
isAda :: AssetClass -> Bool
isAda = (== adaAssetClass)
