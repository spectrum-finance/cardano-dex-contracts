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

module ErgoDex.Contracts.Proxy.Redeem where

import qualified Prelude as Haskell

import           Ledger
import           Ledger.Value                  (assetClassValueOf)
import qualified Ledger.Ada                    as Ada
import           ErgoDex.Contracts.Proxy.Order
import           ErgoDex.Contracts.Pool        (PoolState(..), PoolConfig(..), getPoolInput, readPoolState, findPoolConfig)
import qualified PlutusTx
import           PlutusTx.Prelude

data RedeemConfig = RedeemConfig
   { poolNft   :: AssetClass
   , poolX     :: AssetClass
   , poolY     :: AssetClass
   , poolLp    :: AssetClass
   , exFee     :: Integer
   , rewardPkh :: PubKeyHash
   } deriving stock (Haskell.Show)
PlutusTx.makeIsDataIndexed ''RedeemConfig [('RedeemConfig, 0)]
PlutusTx.makeLift ''RedeemConfig

{-# INLINABLE mkRedeemValidator #-}
mkRedeemValidator :: RedeemConfig -> BuiltinData -> ScriptContext -> Bool
mkRedeemValidator RedeemConfig{..} _ ctx =
    txSignedBy txInfo rewardPkh || (
      traceIfFalse "Invalid pool" validPool &&
      traceIfFalse "Invalid number of inputs" validNumInputs &&
      traceIfFalse "Invalid reward proposition" validRewardProp &&
      traceIfFalse "Unfair execution fee taken" fairFee &&
      traceIfFalse "Insufficient amount of liquidity returned" fairShare
    )

  where
    txInfo = scriptContextTxInfo ctx
    self   = findOrderInput ctx
    pool   = getPoolInput ctx poolNft
    reward = findRewardInput ctx rewardPkh

    poolValue = txOutValue pool

    validPool = assetClassValueOf poolValue poolNft == 1

    validNumInputs = length (txInfoInputs txInfo) == 2

    validRewardProp = maybe False (== rewardPkh) (pubKeyOutput reward)

    selfValue   = txOutValue self
    rewardValue = txOutValue reward

    ps@PoolConfig{..} = case txOutDatum pool of
      Nothing -> traceError "pool input datum hash not found"
      Just h  -> findPoolConfig txInfo h

    outAda        = Ada.getLovelace $ Ada.fromValue rewardValue
    inAda         = Ada.getLovelace $ Ada.fromValue selfValue
    collateralAda = inAda - exFee

    (outX, outY, opAda)
      | isAda poolX =
        let redeemedAda = rx - collateralAda
        in (redeemedAda, ry, redeemedAda)
      | isAda poolY =
        let redeemedAda = ry - collateralAda 
        in (rx, redeemedAda, redeemedAda)
      | otherwise   = (rx, ry, 0)
      where
        rx = assetClassValueOf rewardValue poolX
        ry = assetClassValueOf rewardValue poolY

    fairFee = outAda >= opAda + collateralAda

    inLq = assetClassValueOf selfValue poolLq

    poolState = readPoolState ps pool

    liquidity' = liquidity poolState
    reservesX' = reservesX poolState
    reservesY' = reservesY poolState

    minReturnX = divide (inLq * reservesX') liquidity'
    minReturnY = divide (inLq * reservesY') liquidity'

    fairShare = outX >= minReturnX && outY >= minReturnY
