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
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import qualified Ledger.Ada                       as Ada
import qualified Ledger.Typed.Scripts             as Scripts
import           Ledger.Value                     (AssetClass (..), symbols, assetClassValue)
import           Ledger.Contexts                  (txSignedBy, pubKeyOutput)
import           ErgoDex.Contracts.Types
import           ErgoDex.Contracts.Pool           (PoolState(..), PoolParams(..), mkPoolState, getPoolInput, findPoolDatum)
import qualified PlutusTx
import           PlutusTx.Prelude
import           PlutusTx.IsData.Class
import           Utils

data RedeemDatum = RedeemDatum
   { poolNft   :: Coin Nft
   , exFee     :: Integer
   , rewardPkh :: PubKeyHash
   } deriving stock (Haskell.Show)
PlutusTx.makeIsDataIndexed ''RedeemDatum [('RedeemDatum, 0)]
PlutusTx.makeLift ''RedeemDatum

data ErgoDexRedeem
instance Scripts.ValidatorTypes ErgoDexRedeem where
    type instance RedeemerType ErgoDexRedeem = Redeemer
    type instance DatumType    ErgoDexRedeem = RedeemDatum

{-# INLINABLE mkRedeemValidator #-}
mkRedeemValidator :: RedeemDatum -> ScriptContext -> Bool
mkRedeemValidator RedeemDatum{..} ctx =
    txSignedBy txInfo rewardPkh || (
      traceIfFalse "Invalid pool" validPool &&
      traceIfFalse "Invalid number of inputs" validNumInputs &&
      traceIfFalse "Invalid reward proposition" validRewardProp &&
      traceIfFalse "Unfair execution fee" fairExFee &&
      traceIfFalse "Insufficient amount of tokens returned" sufficientReturn
    )
  where
    txInfo = scriptContextTxInfo ctx
    self   = txInInfoResolved $ findOwnInput' ctx
    pool   = getPoolInput ctx
    reward = (txInfoOutputs txInfo) !! 1 -- reward box is always 2nd output

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

    sufficientReturn =
        outX >= minReturnX && outY >= minReturnY
      where
        (ps@PoolParams{..}, lq) = case txOutDatum pool of
          Nothing -> traceError "pool input datum hash not found"
          Just h  -> findPoolDatum txInfo h

        outX = valueOf rewardValue poolX
        outY = valueOf rewardValue poolY
        inLq    = valueOf selfValue poolLq

        poolState = mkPoolState ps lq pool

        liquidity' = unAmount $ liquidity poolState
        reservesX' = unAmount $ reservesX poolState
        reservesY' = unAmount $ reservesY poolState

        minReturnX = divide (inLq * reservesX') liquidity'
        minReturnY = divide (inLq * reservesY') liquidity'

redeemInstance :: Scripts.TypedValidator ErgoDexRedeem
redeemInstance = Scripts.mkTypedValidator @ErgoDexRedeem
    $$(PlutusTx.compile [|| mkRedeemValidator ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @RedeemDatum @Redeemer
