{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.Contracts.Proxy.Redeem where

import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Value
import qualified PlutusTx
import PlutusTx.Builtins

data RedeemConfig = RedeemConfig
    { poolNft :: AssetClass
    , poolX   :: AssetClass
    , poolY   :: AssetClass
    , poolLp  :: AssetClass
    , exFee   :: Integer
    , rewardPkh :: PubKeyHash
    , stakePkh  :: Maybe PubKeyHash
    }
     deriving stock (Show)

PlutusTx.makeIsDataIndexed ''RedeemConfig [('RedeemConfig, 0)]
