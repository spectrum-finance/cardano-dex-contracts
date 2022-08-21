{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.Contracts.Proxy.Deposit where

import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Value
import qualified PlutusTx
import PlutusTx.Builtins

data DepositConfig = DepositConfig
    { poolNft :: AssetClass
    , tokenA  :: AssetClass
    , tokenB  :: AssetClass
    , tokenLp :: AssetClass
    , exFee   :: Integer
    , rewardPkh :: PubKeyHash
    , stakePkh  :: Maybe PubKeyHash
    , collateralAda :: Integer
    }
    deriving stock (Show)

PlutusTx.makeIsDataIndexed ''DepositConfig [('DepositConfig, 0)]