{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.Contracts.Proxy.Swap (
    SwapConfig (..)
) where

import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Value
import qualified PlutusTx
import PlutusTx.Builtins

data SwapConfig = SwapConfig
    { base    :: AssetClass
    , quote   :: AssetClass
    , poolNft :: AssetClass
    , feeNum  :: Integer
    , exFeePerTokenNum :: Integer
    , exFeePerTokenDen :: Integer
    , rewardPkh  :: PubKeyHash
    , stakePkh   :: Maybe PubKeyHash
    , baseAmount :: Integer
    , minQuoteAmount :: Integer
    }
    deriving stock (Show)

PlutusTx.makeIsDataIndexed ''SwapConfig [('SwapConfig, 0)]