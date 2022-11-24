{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.Contracts.Proxy.LqMining.Simple.Deposit where

import qualified PlutusTx
import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Value

data DepositConfig = DepositConfig
    { expectedNumEpochs  :: Integer
    , bundleKeyCS        :: CurrencySymbol
    , redeemerPkh        :: PubKeyHash
    , vlqAC              :: AssetClass
    , tmpAC              :: AssetClass
    } deriving stock (Show)

PlutusTx.makeIsDataIndexed ''DepositConfig [('DepositConfig, 0)]

data DepositRedeemer = DepositRedeemer
    { poolInIdx      :: Integer
    , depositInIdx   :: Integer
    , redeemerOutIdx :: Integer
    , bundleOutIdx   :: Integer
    } deriving stock (Show)

PlutusTx.makeIsDataIndexed ''DepositRedeemer [('DepositRedeemer, 0)]