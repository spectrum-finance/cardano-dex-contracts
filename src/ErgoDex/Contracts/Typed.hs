module ErgoDex.Contracts.Typed where

import           Ledger
import           Ledger.Value            (flattenValue, assetClassValueOf)
import           Playground.Contract     (FromJSON, Generic, ToJSON, ToSchema)
import           ErgoDex.Contracts.Types
import           ErgoDex.Contracts.Class
import qualified ErgoDex.Contracts.Pool as P
import qualified PlutusTx
import           PlutusTx.Prelude
import           PlutusTx.IsData.Class

data PoolConfig = PoolConfig
  { poolNft    :: Coin Nft
  , poolX      :: Coin X
  , poolY      :: Coin Y
  , poolLq     :: Coin Liquidity
  , poolFeeNum :: Integer
  } deriving (Haskell.Show, Haskell.Eq, Generic, ToJSON, FromJSON, ToSchema)

instance UnliftErased PoolConfig P.PoolConfig where
  lift PoolConfig{..} = P.PoolConfig
    { poolNft    = unCoin poolNft
    , poolX      = unCoin poolX
    , poolY      = unCoin poolY
    , poolLq     = unCoin poolLq
    , poolFeeNum = poolFeeNum
    }

  unlift P.PoolConfig{..} = PoolConfig
    { poolNft    = Coin poolNft
    , poolX      = Coin poolX
    , poolY      = Coin poolY
    , poolLq     = Coin poolLq
    , poolFeeNum = poolFeeNum
    }
