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
import Plutus.V1.Ledger.Api   (PubKeyHash)
import qualified PlutusTx
import PlutusTx.Builtins
import PlutusTx.Prelude

-- Unwrapped representation of PoolConfig
data PoolConfig = PoolConfig
    { poolNft :: AssetClass
    , poolX   :: AssetClass
    , poolY   :: AssetClass
    , poolLq  :: AssetClass
    , poolFeeNum  :: Integer
    , stakeAdmins :: [PubKeyHash]
    , lqBound     :: Integer
    }
    deriving (Haskell.Show, Eq)

PlutusTx.makeIsDataIndexed ''PoolConfig [('PoolConfig, 0)]
PlutusTx.makeLift ''PoolConfig

data PoolAction = Deposit | Redeem | Swap | Destroy | ChangeStakingPool
    deriving (Haskell.Show)
PlutusTx.makeLift ''PoolAction

instance PlutusTx.FromData PoolAction where
    {-# INLINE fromBuiltinData #-}
    fromBuiltinData d = matchData' d (\_ _ -> Nothing) (const Nothing) (const Nothing) chooseAction (const Nothing)
      where
        chooseAction i
            | i == 0 = Just Deposit
            | i == 1 = Just Redeem
            | i == 2 = Just Swap
            | i == 3 = Just Destroy
            | i == 4 = Just ChangeStakingPool
            | otherwise = Nothing

instance PlutusTx.UnsafeFromData PoolAction where
    {-# INLINE unsafeFromBuiltinData #-}
    unsafeFromBuiltinData = maybe (PlutusTx.Builtins.error ()) id . PlutusTx.fromBuiltinData

instance PlutusTx.ToData PoolAction where
    {-# INLINE toBuiltinData #-}
    toBuiltinData a = mkI $ case a of
        Deposit -> 0
        Redeem -> 1
        Swap -> 2
        Destroy -> 3
        ChangeStakingPool -> 4

data PoolRedeemer = PoolRedeemer
    { action :: PoolAction
    , selfIx :: Integer
    }
    deriving (Haskell.Show, Eq, Haskell.Generic)

PlutusTx.makeIsDataIndexed ''PoolRedeemer [('PoolRedeemer, 0)]

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