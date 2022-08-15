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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}

module ErgoDex.Contracts.Pool (
    PoolConfig (..),
    PoolAction (..),
    PoolRedeemer (..),
    PoolState (..),
    maxLqCap,
    maxLqCapAmount,
    burnLqInitial
) where

import qualified Prelude as Haskell

import ErgoDex.Contracts.Types
import qualified GHC.Generics as Haskell
import Plutus.V1.Ledger.Value (AssetClass, assetClassValueOf, flattenValue)
import qualified PlutusTx
import PlutusTx.Builtins
import PlutusTx.Prelude

-- Unwrapped representation of PoolConfig
data PoolConfig = PoolConfig
    { poolNft :: AssetClass
    , poolX   :: AssetClass
    , poolY   :: AssetClass
    , poolLq  :: AssetClass
    , poolFeeNum :: Integer
    }
    deriving (Haskell.Show, Eq)

data PoolAction = Deposit | Redeem | Swap | Destroy
    deriving (Haskell.Show)

data PoolRedeemer = PoolRedeemer
    { action :: PoolAction
    , selfIx :: Integer
    }
    deriving (Haskell.Show, Eq, Haskell.Generic)

data PoolState = PoolState
    { reservesX :: Integer
    , reservesY :: Integer
    , liquidity :: Integer
    }
    deriving (Haskell.Show)

maxLqCap :: Integer
maxLqCap = 0x7fffffffffffffff

burnLqInitial :: Integer
burnLqInitial = 1000

maxLqCapAmount :: Amount Liquidity
maxLqCapAmount = Amount maxLqCap