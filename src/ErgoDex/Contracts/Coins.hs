{-# LANGUAGE NoImplicitPrelude          #-}

module ErgoDex.Contracts.Coins where

import ErgoDex.Contracts.Types
import PlutusTx.Prelude
import ErgoDex.Plutus   (adaAssetClass)

{-# INLINABLE isAda #-}
isAda :: Coin a -> Bool
isAda (Coin cls) = cls == adaAssetClass
