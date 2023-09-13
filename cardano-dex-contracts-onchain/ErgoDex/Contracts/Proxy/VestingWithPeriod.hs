{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.Contracts.Proxy.VestingWithPeriod (
    VestingWithPeriodConfig   (..),
    VestingWithPeriodRedeemer (..)
) where

import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V1.Time
import qualified PlutusTx

data VestingWithPeriodRedeemer = VestingWithPeriodRedeemer
    { vestingInIx      :: Integer
    , vestingPeriodIdx :: Integer
    }
    deriving stock (Show)

PlutusTx.makeIsDataIndexed ''VestingWithPeriodRedeemer [('VestingWithPeriodRedeemer, 0)]

data VestingWithPeriodConfig = VestingWithPeriodConfig
    { vestingStart          :: POSIXTime
    , vestingPeriodDuration :: POSIXTime
    , totalVested           :: Integer
    , periodVested          :: Integer
    , pkhs                  :: [PubKeyHash]
    , vestingAC             :: AssetClass 
    }
    deriving stock (Show)

PlutusTx.makeIsDataIndexed ''VestingWithPeriodConfig [('VestingWithPeriodConfig, 0)]

