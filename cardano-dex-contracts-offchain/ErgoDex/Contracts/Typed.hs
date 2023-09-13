{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ErgoDex.Contracts.Typed where

import qualified Prelude as Haskell

import ErgoDex.Contracts.Class
import qualified ErgoDex.Contracts.Pool as P
import ErgoDex.Contracts.Types
import Plutus.V1.Ledger.Value (CurrencySymbol(..))
import GHC.Generics (Generic)
import PlutusTx.Prelude

data PoolConfig = PoolConfig
    { poolNft :: Coin Nft
    , poolX :: Coin X
    , poolY :: Coin Y
    , poolLq :: Coin Liquidity
    , poolFeeNum :: Integer
    , stakeAdminPolicy :: [CurrencySymbol]
    , lqBound          :: Integer
    }
    deriving (Haskell.Show, Haskell.Eq, Generic)

instance UnliftErased PoolConfig P.PoolConfig where
    lift PoolConfig{..} =
        P.PoolConfig
            { poolNft = unCoin poolNft
            , poolX = unCoin poolX
            , poolY = unCoin poolY
            , poolLq = unCoin poolLq
            , poolFeeNum = poolFeeNum
            , stakeAdminPolicy = stakeAdminPolicy
            , lqBound = lqBound
            }

    unlift P.PoolConfig{..} =
        PoolConfig
            { poolNft = Coin poolNft
            , poolX = Coin poolX
            , poolY = Coin poolY
            , poolLq = Coin poolLq
            , poolFeeNum = poolFeeNum
            , stakeAdminPolicy = stakeAdminPolicy
            , lqBound = lqBound
            }
