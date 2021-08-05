{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE DuplicateRecordFields      #-}

module Proxy.Contract.OffChain where

import           Playground.Contract
import           Plutus.Contract
import           Ledger                           hiding (singleton)
import qualified PlutusTx
import           PlutusTx.Prelude                 hiding (Semigroup (..), dropWhile, flip, unless)
import           Prelude                          as Haskell (Int, Semigroup (..), String, div, dropWhile, flip, show,
                                                              (^), undefined)
import           Data.Proxy                       (Proxy (..))
import           Data.Void                        (Void, absurd)
import qualified PlutusTx.Builtins                as Builtins
import           Data.Text                        (Text, pack)
import           Data.Monoid                      (Last (..))
import           Proxy.Contract.OnChain



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
} deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data RedeemParams = RedeemParams {
    toGetCoin :: AssetClass,
    toSwapCoin :: AssetClass,
    lpProxyToken :: AssetClass,
    dexFeeDatum :: Integer,
    targetPoolId :: Builtins.ByteString
} deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data DepositParams = DepositParams {
    toGetCoin :: AssetClass,
    toSwapCoin :: AssetClass,
    lpProxyToken :: AssetClass,
    dexFeeDatum :: Integer,
    targetPoolId :: Builtins.ByteString
} deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data Order = SwapOrder SwapParams | RedeemOrder RedeemParams | DepositOrder DepositParams
    deriving (Show, Generic, FromJSON, ToJSON)

data UserContractState =
      Orders [Order]
    deriving (Show, Generic, FromJSON, ToJSON)

proxyAddress :: Ledger.Address
proxyAddress = scriptAddress proxyValidator

proxyHash :: ValidatorHash
proxyHash = validatorHash proxyValidator

swap :: SwapParams -> Contract w s Text [Order]
swap _ = return []

redeem :: RedeemParams -> Contract w s Text [Order]
redeem _ = return []

deposit :: DepositParams -> Contract w s Text [Order]
deposit _ = return []

orders :: Contract w s Text [Order]
orders = return []

userEndpoints :: Contract (Last (Either Text UserContractState)) ProxyUserSchema Void ()
userEndpoints =
    ((f (Proxy @"swap")     Orders       swap                               `select` 
      f (Proxy @"redeem")   Orders       redeem                             `select` 
      f (Proxy @"deposit")  Orders       deposit                            `select` 
      f (Proxy @"orders")   Orders       (\_ -> orders)                   ) >> userEndpoints)
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
            c p
        tell $ Last $ Just $ case e of
            Left err -> Left err
            Right a  -> Right $ g a