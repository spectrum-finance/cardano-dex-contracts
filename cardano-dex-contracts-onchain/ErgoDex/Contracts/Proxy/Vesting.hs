{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.Contracts.Proxy.Vesting (
    VestingConfig   (..),
    VestingRedeemer (..)
) where

import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V1.Time
import qualified PlutusTx

data VestingRedeemer = VestingRedeemer
    { vestingInIx :: Integer
    , rewardOutIx :: Integer
    }
    deriving stock (Show)

PlutusTx.makeIsDataIndexed ''VestingRedeemer [('VestingRedeemer, 0)]

data VestingConfig = VestingConfig
    { deadline  :: POSIXTime
    , pkh       :: PubKeyHash
    , vestingAC :: AssetClass 
    }
    deriving stock (Show)

PlutusTx.makeIsDataIndexed ''VestingConfig [('VestingConfig, 0)]

