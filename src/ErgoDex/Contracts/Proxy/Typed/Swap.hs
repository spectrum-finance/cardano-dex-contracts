module ErgoDex.Contracts.Proxy.Typed.Swap where

import qualified Prelude                          as Haskell

import           Ledger
import qualified ErgoDex.Contracts.Proxy.Swap  as S
import           ErgoDex.Contracts.Class
import           ErgoDex.Contracts.Types
import           PlutusTx.Prelude

data SwapConfig = SwapConfig
   { base             :: Coin Base
   , quote            :: Coin Quote
   , poolNft          :: Coin Nft
   , feeNum           :: Integer
   , exFeePerTokenNum :: Integer
   , exFeePerTokenDen :: Integer
   , rewardPkh        :: PubKeyHash
   , stakePkh         :: Haskell.Maybe PubKeyHash
   , baseAmount       :: Amount Base
   , minQuoteAmount   :: Amount Quote
   } deriving stock (Haskell.Show)

instance UnliftErased SwapConfig S.SwapConfig where
  lift SwapConfig{..} = S.SwapConfig
    { base             = unCoin base
    , quote            = unCoin quote
    , poolNft          = unCoin poolNft
    , feeNum           = feeNum
    , exFeePerTokenNum = exFeePerTokenNum
    , exFeePerTokenDen = exFeePerTokenDen
    , rewardPkh        = rewardPkh
    , baseAmount       = unAmount baseAmount
    , minQuoteAmount   = unAmount minQuoteAmount
    }

  unlift S.SwapConfig{..} = SwapConfig
    { base             = Coin base
    , quote            = Coin quote
    , poolNft          = Coin poolNft
    , feeNum           = feeNum
    , exFeePerTokenNum = exFeePerTokenNum
    , exFeePerTokenDen = exFeePerTokenDen
    , rewardPkh        = rewardPkh
    , baseAmount       = Amount baseAmount
    , minQuoteAmount   = Amount minQuoteAmount
    }
