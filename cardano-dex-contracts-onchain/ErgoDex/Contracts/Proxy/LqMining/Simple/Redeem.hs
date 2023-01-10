{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.Contracts.Proxy.LqMining.Simple.Redeem where

import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V1.Crypto (PubKeyHash)

import qualified PlutusTx

data RedeemConfig = RedeemConfig
    { expectedLQAC     :: AssetClass
    , expectedLQAmount :: Integer
    , rewardPkh        :: PubKeyHash
    } deriving stock (Show)

PlutusTx.makeIsDataIndexed ''RedeemConfig [('RedeemConfig, 0)]

data RedeemRedeemerConfig = RedeemRedeemerConfig
    { rewardOutIdx :: Integer 
    } deriving stock (Show)

PlutusTx.makeIsDataIndexed ''RedeemRedeemerConfig [('RedeemRedeemerConfig, 0)]