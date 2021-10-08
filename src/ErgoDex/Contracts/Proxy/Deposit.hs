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

module ErgoDex.Contracts.Proxy.Deposit where

import qualified Prelude                          as Haskell

import           Ledger
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import qualified Ledger.Ada                       as Ada
import           Ledger.Value                     (AssetClass (..), symbols, assetClassValue)
import           Ledger.Contexts                  (txSignedBy, pubKeyOutput)
import qualified Ledger.Typed.Scripts             as Scripts
import           ErgoDex.Contracts.Proxy.Order
import           ErgoDex.Contracts.Types
import           ErgoDex.Contracts.Pool           (PoolState(..), PoolParams(..), mkPoolState, getPoolInput, findPoolDatum)
import qualified PlutusTx
import           PlutusTx.Prelude
import           PlutusTx.IsData.Class
import           Utils

data DepositDatum = DepositDatum
   { poolNft   :: Coin Nft
   , exFee     :: Integer
   , rewardPkh :: PubKeyHash
   } deriving stock (Haskell.Show)
PlutusTx.makeIsDataIndexed ''DepositDatum [('DepositDatum, 0)]
PlutusTx.makeLift ''DepositDatum

{-# INLINABLE mkDepositValidator #-}
mkDepositValidator :: DepositDatum -> BuiltinData -> ScriptContext -> Bool
mkDepositValidator DepositDatum{..} _ ctx =
    txSignedBy txInfo rewardPkh || (
      traceIfFalse "Invalid pool" validPool &&
      traceIfFalse "Invalid number of inputs" validNumInputs &&
      traceIfFalse "Invalid reward proposition" validRewardProp &&
      traceIfFalse "Unfair execution fee" fairExFee &&
      traceIfFalse "Minimal reward not met" validReward
    )
  where
    txInfo = scriptContextTxInfo ctx
    self   = getOrderInput ctx
    pool   = getPoolInput ctx
    reward = getOrderRewardOutput ctx

    poolValue = txOutValue pool

    validPool = isUnit poolValue poolNft

    validNumInputs = (length $ txInfoInputs txInfo) == 2

    validRewardProp = maybe False (== rewardPkh) (pubKeyOutput reward)

    selfValue   = txOutValue self
    rewardValue = txOutValue reward

    fairExFee =
        outAda >= inAda - exFee
      where
        outAda = Ada.getLovelace $ Ada.fromValue rewardValue
        inAda  = Ada.getLovelace $ Ada.fromValue selfValue

    validReward =
        outLq >= minReward
      where
        (ps@PoolParams{..}, lq) = case txOutDatum pool of
          Nothing -> traceError "pool input datum hash not found"
          Just h  -> findPoolDatum txInfo h

        inX   = valueOf selfValue poolX
        inY   = valueOf selfValue poolY
        outLq = valueOf rewardValue poolLq

        poolState = mkPoolState ps lq pool

        liquidity' = unAmount $ liquidity poolState
        reservesX' = unAmount $ reservesX poolState
        reservesY' = unAmount $ reservesY poolState

        minReward = min (divide (inX * liquidity') reservesX') (divide (inY * liquidity') reservesY')
