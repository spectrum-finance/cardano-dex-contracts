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

import qualified Prelude                          as Haskell

import           Ledger
import qualified Ledger.Ada                       as Ada
import           ErgoDex.Contracts.Proxy.Order
import           ErgoDex.Contracts.Types
import           ErgoDex.Contracts.Pool           (PoolState(..), PoolParams(..), mkPoolState, getPoolInput, findPoolDatum)
import qualified PlutusTx
import           PlutusTx.Prelude

data RedeemDatum = RedeemDatum
   { poolNft   :: Coin Nft
   , exFee     :: Amount Lovelace
   , rewardPkh :: PubKeyHash
   } deriving stock (Haskell.Show)
PlutusTx.makeIsDataIndexed ''RedeemDatum [('RedeemDatum, 0)]
PlutusTx.makeLift ''RedeemDatum

{-# INLINABLE mkRedeemValidator #-}
mkRedeemValidator :: RedeemDatum -> BuiltinData -> ScriptContext -> Bool
mkRedeemValidator RedeemDatum{..} _ ctx =
    txSignedBy txInfo rewardPkh || (
      traceIfFalse "Invalid pool" validPool &&
      traceIfFalse "Invalid number of inputs" validNumInputs &&
      traceIfFalse "Invalid reward proposition" validRewardProp &&
      traceIfFalse "Unfair execution fee taken" fairFee &&
      traceIfFalse "Insufficient amount of liquidity returned" fairShare
    )
  where
    txInfo = scriptContextTxInfo ctx
    self   = getOrderInput ctx
    pool   = getPoolInput ctx
    reward = getOrderRewardOutput ctx

    poolValue = txOutValue pool

    validPool = isUnit poolValue poolNft

    validNumInputs = length (txInfoInputs txInfo) == 2

    validRewardProp = maybe False (== rewardPkh) (pubKeyOutput reward)

    selfValue   = txOutValue self
    rewardValue = txOutValue reward

    (ps@PoolParams{..}, lq) = case txOutDatum pool of
      Nothing -> traceError "pool input datum hash not found"
      Just h  -> findPoolDatum txInfo h

    outAda        = Ada.getLovelace $ Ada.fromValue rewardValue
    inAda         = Ada.getLovelace $ Ada.fromValue selfValue
    exFee'        = unAmount exFee
    collateralAda = inAda - exFee'

    (outX, outY, opAda)
      | isAda poolX =
        let redeemedAda = rx - collateralAda
        in (redeemedAda, ry, redeemedAda)
      | isAda poolY =
        let redeemedAda = ry - collateralAda 
        in (rx, redeemedAda, redeemedAda)
      | otherwise   = (rx, ry, 0)
      where
          rx = valueOf rewardValue poolX
          ry = valueOf rewardValue poolY

    fairFee = outAda >= opAda + collateralAda

    inLq = valueOf selfValue poolLq

    poolState = mkPoolState ps lq pool

    liquidity' = unAmount $ liquidity poolState
    reservesX' = unAmount $ reservesX poolState
    reservesY' = unAmount $ reservesY poolState

    minReturnX = divide (inLq * reservesX') liquidity'
    minReturnY = divide (inLq * reservesY') liquidity'

    fairShare = outX >= minReturnX && outY >= minReturnY
