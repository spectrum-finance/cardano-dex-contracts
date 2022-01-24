module ErgoDex.Contracts.Proxy.Typed.Swap where

import qualified Prelude                          as Haskell

import           Ledger
import qualified Ledger.Ada                       as Ada
import           ErgoDex.Contracts.Proxy.Order
import           ErgoDex.Contracts.Types
import           ErgoDex.Contracts.Pool           (getPoolInput)
import qualified PlutusTx
import           PlutusTx.Prelude

data SwapConfig = SwapConfig
   { base             :: Coin Base
   , quote            :: Coin Quote
   , poolNft          :: Coin Nft
   , feeNum           :: Integer
   , exFeePerTokenNum :: Integer
   , exFeePerTokenDen :: Integer
   , rewardPkh        :: PubKeyHash
   , baseAmount       :: Amount Base
   , minQuoteAmount   :: Amount Quote
   } deriving stock (Haskell.Show)