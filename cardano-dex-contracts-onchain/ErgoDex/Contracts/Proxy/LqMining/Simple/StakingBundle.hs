{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.Contracts.Proxy.LqMining.Simple.StakingBundle where

import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V1.Crypto (PubKeyHash)

import qualified PlutusTx

data StakingBundleConfig = StakingBundleConfig
    { bundleAC    :: AssetClass
    , poolAC      :: AssetClass
    , bundleLQAC  :: AssetClass
    , bundleVLQAC :: AssetClass
    , bundleTMPAC :: AssetClass
    , redeemerPkh :: PubKeyHash
    } deriving stock (Show)

PlutusTx.makeIsDataIndexed ''StakingBundleConfig [('StakingBundleConfig, 0)]

data StakingBundleRedeemer = StakingBundleRedeemer
    { poolInIdx    :: Integer
    , permitIdx    :: Integer
    , selfInIdx    :: Integer
    , redeemerOutIx :: Integer
    , successorOutIndex :: Integer
    } deriving stock (Show)

PlutusTx.makeIsDataIndexed ''StakingBundleRedeemer [('StakingBundleRedeemer, 0)]