module ErgoDex.Contracts.Proxy.Typed.Redeem where

import qualified Prelude as Haskell

import           Ledger
import qualified ErgoDex.Contracts.Proxy.Redeem as R
import           ErgoDex.Contracts.Class
import           ErgoDex.Contracts.Types

data RedeemConfig = RedeemConfig
   { poolNft   :: Coin Nft
   , exFee     :: Amount Lovelace
   , rewardPkh :: PubKeyHash
   } deriving stock (Haskell.Show)

instance UnliftErased RedeemConfig R.RedeemConfig where
  lift RedeemConfig{..} = R.RedeemConfig
    { poolNft   = unCoin poolNft
    , exFee     = unAmount exFee
    , rewardPkh = rewardPkh
    }

  unlift R.RedeemConfig{..} = RedeemConfig
    { poolNft   = Coin poolNft
    , exFee     = Amount exFee
    , rewardPkh = rewardPkh
    }
