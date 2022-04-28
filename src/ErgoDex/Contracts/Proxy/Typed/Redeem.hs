module ErgoDex.Contracts.Proxy.Typed.Redeem where

import qualified Prelude as Haskell

import           Ledger
import qualified ErgoDex.Contracts.Proxy.Redeem as R
import           ErgoDex.Contracts.Class
import           ErgoDex.Contracts.Types

data RedeemConfig = RedeemConfig
   { poolNft   :: Coin Nft
   , poolX     :: Coin X
   , poolY     :: Coin Y
   , poolLp    :: Coin Liquidity
   , exFee     :: Amount Lovelace
   , rewardPkh :: PubKeyHash
   , stakePkh  :: Haskell.Maybe PubKeyHash
   } deriving stock (Haskell.Show)

instance UnliftErased RedeemConfig R.RedeemConfig where
  lift RedeemConfig{..} = R.RedeemConfig
    { poolNft   = unCoin poolNft
    , poolX     = unCoin poolX
    , poolY     = unCoin poolY
    , poolLp    = unCoin poolLp
    , exFee     = unAmount exFee
    , rewardPkh = rewardPkh
    }

  unlift R.RedeemConfig{..} = RedeemConfig
    { poolNft   = Coin poolNft
    , poolX     = Coin poolX
    , poolY     = Coin poolY
    , poolLp    = Coin poolLp
    , exFee     = Amount exFee
    , rewardPkh = rewardPkh
    }
