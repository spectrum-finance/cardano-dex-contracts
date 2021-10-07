{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module ErgoDex.OffChain where

import           ErgoDex.Contracts.Pool (PoolDatum, PoolAction, mkPoolValidator)
import           ErgoDex.Contracts.Proxy.Deposit
import           ErgoDex.Contracts.Proxy.Swap
import           ErgoDex.Contracts.Proxy.Redeem
import           ErgoDex.Contracts.Liquidity
import           Ledger.Scripts                (mkMintingPolicyScript)
import           ErgoDex.Contracts.Types
import qualified Ledger.Typed.Scripts as Scripts
import qualified PlutusTx

data ErgoDexPool
instance Scripts.ValidatorTypes ErgoDexPool where
    type instance RedeemerType ErgoDexPool = PoolAction
    type instance DatumType    ErgoDexPool = PoolDatum

poolInstance :: Scripts.TypedValidator ErgoDexPool
poolInstance = Scripts.mkTypedValidator @ErgoDexPool
    ($$(PlutusTx.compile [|| mkPoolValidator ||]))
     $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @PoolDatum @PoolAction

data ErgoDexDeposit
instance Scripts.ValidatorTypes ErgoDexDeposit where
    type instance RedeemerType ErgoDexDeposit = PlutusTx.BuiltinData
    type instance DatumType ErgoDexDeposit = DepositDatum

depositInstance :: Scripts.TypedValidator ErgoDexDeposit
depositInstance = Scripts.mkTypedValidator @ErgoDexDeposit
    ($$(PlutusTx.compile [|| mkDepositValidator ||]))
     $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @DepositDatum

data ErgoDexSwap
instance Scripts.ValidatorTypes ErgoDexSwap where
    type instance RedeemerType ErgoDexSwap = PlutusTx.BuiltinData
    type instance DatumType ErgoDexSwap = SwapDatum

swapInstance :: Scripts.TypedValidator ErgoDexSwap
swapInstance = Scripts.mkTypedValidator @ErgoDexSwap
    ($$(PlutusTx.compile [|| mkSwapValidator ||]))
     $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @SwapDatum

data ErgoDexRedeem
instance Scripts.ValidatorTypes ErgoDexRedeem where
    type instance RedeemerType ErgoDexRedeem = PlutusTx.BuiltinData
    type instance DatumType ErgoDexRedeem = RedeemDatum

redeemInstance :: Scripts.TypedValidator ErgoDexRedeem
redeemInstance = Scripts.mkTypedValidator @ErgoDexRedeem
    ($$(PlutusTx.compile [|| mkRedeemValidator ||]))
     $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @RedeemDatum

liquidityMintingPolicyInstance :: Coin Nft -> Scripts.MintingPolicy
liquidityMintingPolicyInstance coin = mkMintingPolicyScript (
      $$(PlutusTx.compile [|| \c -> Scripts.wrapMintingPolicy (validateLiquidityMinting c) ||])
          `PlutusTx.applyCode` PlutusTx.liftCode coin )
