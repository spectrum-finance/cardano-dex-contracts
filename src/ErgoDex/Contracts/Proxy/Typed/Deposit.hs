module ErgoDex.Contracts.Proxy.Typed.Deposit where

import qualified Prelude as Haskell

import           Ledger
import qualified ErgoDex.Contracts.Proxy.Deposit as D
import           ErgoDex.Contracts.Class
import           ErgoDex.Contracts.Types

data DepositConfig = DepositConfig
   { poolNft       :: Coin Nft
   , tokenA        :: Coin X
   , tokenB        :: Coin Y
   , tokenLp       :: Coin Liquidity
   , exFee         :: Amount Lovelace
   , rewardPkh     :: PubKeyHash
   , stakePkh      :: Haskell.Maybe PubKeyHash
   , collateralAda :: Amount Lovelace
   } deriving stock (Haskell.Show)

instance UnliftErased DepositConfig D.DepositConfig where
  lift DepositConfig{..} = D.DepositConfig
    { poolNft       = unCoin poolNft
    , tokenA        = unCoin tokenA
    , tokenB        = unCoin tokenB
    , tokenLp       = unCoin tokenLp
    , exFee         = unAmount exFee
    , rewardPkh     = rewardPkh
    , collateralAda = unAmount collateralAda
    }

  unlift D.DepositConfig{..} = DepositConfig
    { poolNft       = Coin poolNft
    , tokenA        = Coin tokenA
    , tokenB        = Coin tokenB
    , tokenLp       = Coin tokenLp
    , exFee         = Amount exFee
    , rewardPkh     = rewardPkh
    , collateralAda = Amount collateralAda
    }
