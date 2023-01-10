{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.Contracts.Proxy.LqMining.Simple.LMPool where

import qualified PlutusTx
import PlutusLedgerApi.V1.Value

data LMPoolConfig = LMPoolConfig
    { epochLen      :: Integer
    , epochNum      :: Integer
    , programStart  :: Integer
    , programBudget :: Integer
    , execBudget    :: Integer
    , epoch         :: Integer
    , maxRoundingError :: Integer
    , poolNft       :: AssetClass
    , poolX         :: AssetClass
    , poolLQ        :: AssetClass
    , poolVLQ       :: AssetClass
    , poolTMP       :: AssetClass
    } deriving stock (Show)

PlutusTx.makeIsDataIndexed ''LMPoolConfig [('LMPoolConfig, 0)]

data LMPoolRedeemer = LMPoolRedeemer
    { poolInIdx   :: Integer
    , poolOutIdx  :: Integer
    } deriving stock (Show)

PlutusTx.makeIsDataIndexed ''LMPoolRedeemer [('LMPoolRedeemer, 0)]