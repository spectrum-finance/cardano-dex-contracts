module ErgoDex.Contracts.Proxy.Typed.Redeem where

import qualified Prelude as Haskell

import           Ledger
import qualified Ledger.Ada                    as Ada
import           ErgoDex.Contracts.Proxy.Order
import           ErgoDex.Contracts.Types
import           ErgoDex.Contracts.Pool        (PoolState(..), PoolConfig(..), getPoolInput, readPoolState, findPoolConfig)
import qualified PlutusTx
import           PlutusTx.Prelude

data RedeemConfig = RedeemConfig
   { poolNft   :: Coin Nft
   , exFee     :: Amount Lovelace
   , rewardPkh :: PubKeyHash
   } deriving stock (Haskell.Show)
