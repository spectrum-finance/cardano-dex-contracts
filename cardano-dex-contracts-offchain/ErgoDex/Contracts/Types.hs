{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}

module ErgoDex.Contracts.Types where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import ErgoDex.Plutus (adaAssetClass)
import Plutus.V1.Ledger.Value (AssetClass (..), Value (..), assetClassValue, assetClassValueOf)
import qualified PlutusTx
import PlutusTx.Prelude
import Text.Printf (PrintfArg)
import qualified Prelude as Haskell

data Lovelace = Lovelace deriving (Haskell.Show, Haskell.Eq, Generic)

-- Pool NFT
data Nft = Nft deriving (Haskell.Show, Haskell.Eq, Generic)

-- First asset of a pool
data X = X deriving (Haskell.Show, Haskell.Eq, Generic)

-- Second asset of a pool
data Y = Y deriving (Haskell.Show, Haskell.Eq, Generic)

-- Liquidity token of a pool
data Liquidity = Liquidity deriving (Haskell.Show, Haskell.Eq, Generic)

-- First asset of a pool
data Quote = Quote deriving (Haskell.Show, Haskell.Eq, Generic)

-- Second asset of a pool
data Base = Base deriving (Haskell.Show, Haskell.Eq, Generic)

-- Type to distinguish tokens within a pool
newtype Coin a = Coin {unCoin :: AssetClass}
    deriving stock (Haskell.Show, Generic)
    deriving newtype (ToJSON, FromJSON, Eq, Haskell.Eq, Haskell.Ord)
    deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

{-# INLINEABLE retagCoin #-}
retagCoin :: forall a b. Coin a -> Coin b
retagCoin (Coin ac) = Coin ac

{-# INLINEABLE valueOf #-}
valueOf :: Value -> Coin a -> Integer
valueOf v = assetClassValueOf v . unCoin

-- Difference of a token amount
newtype Diff a = Diff {unDiff :: Integer}
    deriving stock (Haskell.Show, Generic)
    deriving newtype (ToJSON, FromJSON, Eq, Ord, PrintfArg)
    deriving newtype (Haskell.Eq, Haskell.Ord, Haskell.Num)
    deriving newtype (AdditiveGroup, AdditiveMonoid, AdditiveSemigroup, MultiplicativeSemigroup)
    deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

-- Amount of a token
newtype Amount a = Amount {unAmount :: Integer}
    deriving stock (Haskell.Show, Generic)
    deriving newtype (ToJSON, FromJSON, Eq, Ord, PrintfArg)
    deriving newtype (Haskell.Eq, Haskell.Ord, Haskell.Num)
    deriving newtype (AdditiveGroup, AdditiveMonoid, AdditiveSemigroup, MultiplicativeSemigroup)
    deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

{-# INLINEABLE amountOf #-}
amountOf :: Value -> Coin a -> Amount a
amountOf v = Amount . assetClassValueOf v . unCoin

{-# INLINEABLE isUnit #-}
isUnit :: Value -> Coin a -> Bool
isUnit v c = amountOf v c == 1

{-# INLINEABLE coinAmountValue #-}
coinAmountValue :: Coin a -> Amount a -> Value
coinAmountValue (Coin ac) (Amount v) = assetClassValue ac v

{-# INLINEABLE isAda #-}
isAda :: Coin a -> Bool
isAda (Coin cls) = cls == adaAssetClass
