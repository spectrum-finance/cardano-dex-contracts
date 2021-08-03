

module Proxy.Contract.Offchain where

import           Playground.Contract
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude                 hiding (Semigroup (..), dropWhile, flip, unless)
import           Prelude                          as Haskell (Int, Semigroup (..), String, div, dropWhile, flip, show,
                                                              (^))
import           Data.Proxy                       (Proxy (..))

type ProxyUserSchema =
        Endpoint "swap" SwapParams
        .\/ Endpoint "redeem"   RedeemParams
        .\/ Endpoint "deposit"  DepositParams
        .\/ Endpoint "orders"   ()

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

data Order = SwapOrder SwapParams | RedeemOrder RedeemParams | DepositOrder DepositParams

data UserContractState =
      Orders [Order]
    deriving (Show, Generic, FromJSON, ToJSON)

swap :: SwapParams -> Contract w s Text ()
swap = undefined

redeem :: RedeemParams -> Contract w s Text ()
redeem = undefined

deposit :: DepositParams -> Contract w s Text ()
deposit = undefined

orders :: Contract w s Text [Order]
orders = undefined

userEndpoints :: Contract (Last (Either Text UserContractState)) ProxyUserSchema Void ()
userEndpoints us =
    stop
        `select`
    ((f (Proxy @"swap")     Orders       swap                       `select`
      f (Proxy @"redeem")   Orders       redeem                     `select`
      f (Proxy @"deposit")  Orders       deposit                    `select`
      f (Proxy @"pools")    Orders       (\us' () -> orders us'))    >> userEndpoints us)
  where
    f :: forall l a p.
         (HasEndpoint l p ProxyUserSchema, FromJSON p)
      => Proxy l
      -> (a -> UserContractState)
      -> (p -> Contract (Last (Either Text UserContractState)) ProxyUserSchema Text a)
      -> Contract (Last (Either Text UserContractState)) ProxyUserSchema Void ()
    f _ g c = do
        e <- runError $ do
            p <- endpoint @l
            c us p
        tell $ Last $ Just $ case e of
            Left err -> Left err
            Right a  -> Right $ g a

    stop :: Contract (Last (Either Text UserContractState)) ProxyUserSchema Void ()
    stop = do
        e <- runError $ endpoint @"stop"
        tell $ Last $ Just $ case e of
            Left err -> Left err
            Right () -> Right Stopped