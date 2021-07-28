

module Proxy.Contract.Offchain where

import           Playground.Contract
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude                 hiding (Semigroup (..), dropWhile, flip, unless)
import           Prelude                          as Haskell (Int, Semigroup (..), String, div, dropWhile, flip, show,
                                                              (^))

type ProxyUserSchema =
        Endpoint "swap" SwapParams
        .\/ Endpoint "redeem"   RedeemParams
        .\/ Endpoint "deposit"  DepositParams

data SwapParams = SwapParams {
    toGetCoin :: AssetClass,
    toSwapCoin :: AssetClass,
    lpProxyToken :: AssetClass,
    dexFeeDatum :: Integer,
    targetPoolId :: Builtins.ByteString
}

data RedeemParams = RedeemParams {
    toGetCoin :: AssetClass,
    toSwapCoin :: AssetClass,
    lpProxyToken :: AssetClass,
    dexFeeDatum :: Integer,
    targetPoolId :: Builtins.ByteString,
}

data DepositParams = DepositParams {
    toGetCoin :: AssetClass,
    toSwapCoin :: AssetClass,
    lpProxyToken :: AssetClass,
    dexFeeDatum :: Integer,
    targetPoolId :: Builtins.ByteString
}

swap :: SwapParams -> Contract w s Text ()
swap = undefined

redeem :: RedeemParams -> Contract w s Text ()
redeem = undefined

deposit :: DepositParams -> Contract w s Text ()
deposit = undefined