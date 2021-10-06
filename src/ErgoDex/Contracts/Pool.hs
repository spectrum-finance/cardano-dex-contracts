{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module ErgoDex.Contracts.Pool where

import           Ledger
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import           Ledger.Value                     (AssetClass (..), symbols, assetClassValue, isZero, flattenValue)
import           ErgoDex.Contracts.Types
import qualified PlutusTx
import           PlutusTx.Prelude
import           PlutusTx.IsData.Class
import           Utils

data PoolState = PoolState
  { reservesX :: Amount X
  , reservesY :: Amount Y
  , liquidity :: Amount Liquidity
  }

{-# INLINABLE mkPoolState #-}
mkPoolState :: PoolParams -> Amount Liquidity -> TxOut -> PoolState
mkPoolState PoolParams{..} lq out =
    PoolState x y lq
  where
    value = txOutValue out
    x     = amountOf value poolX
    y     = amountOf value poolY

data PoolDiff = PoolDiff
  { diffX         :: Diff X
  , diffY         :: Diff Y
  , diffLiquidity :: Diff Liquidity
  }

{-# INLINABLE diffPoolState #-}
diffPoolState :: PoolState -> PoolState -> PoolDiff
diffPoolState s0 s1 =
    PoolDiff dx dy dlq
  where
    rx0 = unAmount $ reservesX s0
    rx1 = unAmount $ reservesX s1
    ry0 = unAmount $ reservesY s0
    ry1 = unAmount $ reservesY s1
    l0  = unAmount $ liquidity s0
    l1  = unAmount $ liquidity s1
    dx  = Diff $ rx1 - rx0
    dy  = Diff $ ry1 - ry0
    dlq = Diff $ l1 - l0

{-# INLINABLE validDeposit #-}
validDeposit :: PoolState -> PoolDiff -> Bool
validDeposit PoolState{..} PoolDiff{..} =
    traceIfFalse "Illegal amount of liquidity forged" (diffLiquidity' <= liquidityUnlocked)
  where
    diffLiquidity'    = unDiff diffLiquidity
    diffX'            = unDiff diffX
    diffY'            = unDiff diffY
    liquidity'        = unAmount liquidity
    reservesX'        = unAmount reservesX
    reservesY'        = unAmount reservesY

    liquidityUnlocked = min (divide (diffX' * liquidity') reservesX') (divide (diffY' * liquidity') reservesY')

{-# INLINABLE validRedeem #-}
validRedeem :: PoolState -> PoolDiff -> Bool
validRedeem PoolState{..} PoolDiff{..} =
    traceIfFalse "Illegal redeem" fairRedeem
  where
    diffLiquidity' = unDiff diffLiquidity
    diffX'         = unDiff diffX
    diffY'         = unDiff diffY
    liquidity'     = unAmount liquidity
    reservesX'     = unAmount reservesX
    reservesY'     = unAmount reservesY

    fairRedeem =
      diffX' * liquidity' >= diffLiquidity' * reservesX' && diffY' * liquidity' >= diffLiquidity' * reservesY'

{-# INLINABLE validSwap #-}
validSwap :: PoolParams -> PoolState -> PoolDiff -> Bool
validSwap PoolParams{..} PoolState{..} PoolDiff{..} =
    traceIfFalse "Illegal swap" fairSwap &&
    traceIfFalse "Liquidity emission must not change" (diffLiquidity == 0)
  where
    reservesX0     = unAmount reservesX
    reservesY0     = unAmount reservesY
    deltaReservesX = unDiff diffX
    deltaReservesY = unDiff diffY
    feeDen         = 1000

    fairSwap =
      if deltaReservesX > 0 then
        reservesY0 * deltaReservesX * feeNum >= -deltaReservesY * (reservesX0 * feeDen + deltaReservesX * feeNum)
      else
        reservesX0 * deltaReservesY * feeNum >= -deltaReservesX * (reservesY0 * feeDen + deltaReservesY * feeNum)

{-# INLINABLE getPoolOutput #-}
getPoolOutput :: ScriptContext -> TxOut
getPoolOutput ScriptContext{scriptContextTxInfo=TxInfo{txInfoOutputs}} =
  head txInfoOutputs

{-# INLINABLE findPoolDatum #-}
findPoolDatum :: TxInfo -> DatumHash -> (PoolParams, Amount Liquidity)
findPoolDatum info h = case findDatum h info of
  Just (Datum d) -> case fromData d of
    (Just (PoolDatum ps lq)) -> (ps, lq)
    _                        -> traceError "error decoding data"
  _              -> traceError "pool input datum not found"

mkPoolValidator :: PoolDatum -> PoolAction -> ScriptContext -> Bool
mkPoolValidator (PoolDatum ps0@PoolParams{..} lq0) action ctx =
    traceIfFalse "Pool NFT not preserved" poolNftPreserved &&
    traceIfFalse "Pool params not preserved" poolParamsPreserved &&
    traceIfFalse "Illegal amount of liquidity declared" liquiditySynced &&
    traceIfFalse "Assets qty not preserved" strictAssets &&
    traceIfFalse "Script not preserved" scriptPreserved &&
    traceIfFalse "Invalid action" validAction
  where
    txInfo    = scriptContextTxInfo ctx
    self      = txInInfoResolved $ findOwnInput' ctx
    successor = getPoolOutput ctx

    poolNftPreserved = isUnit (txOutValue successor) poolNft

    (ps1, lq1) = case txOutDatum successor of
      Nothing -> traceError "pool output datum hash not found"
      Just h  -> findPoolDatum txInfo h

    poolParamsPreserved = ps1 == ps0

    s0   = mkPoolState ps0 lq0 self
    s1   = mkPoolState ps1 lq1 successor
    diff = diffPoolState s0 s1

    valueForged = txInfoForge txInfo

    liquiditySynced = isZero valueForged ||
                      valueForged == (assetClassValue (unCoin poolLq) (unDiff $ diffLiquidity diff))

    numAssets    = length $ flattenValue (txOutValue successor)
    strictAssets = numAssets == 3

    scriptPreserved = (txOutAddress successor) == (txOutAddress self)

    validAction = case action of
      Deposit -> validDeposit s0 diff
      Redeem  -> validRedeem s0 diff
      Swap    -> validSwap ps0 s0 diff
