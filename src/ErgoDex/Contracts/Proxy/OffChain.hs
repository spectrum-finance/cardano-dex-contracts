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

module ErgoDex.Contracts.Proxy.OffChain where

import           ErgoDex.Contracts.Proxy.Deposit
import           ErgoDex.Contracts.Proxy.Swap
import           ErgoDex.Contracts.Proxy.Redeem

import qualified Ledger.Typed.Scripts as Scripts
import qualified PlutusTx

data ErgoDexDeposit
instance Scripts.ValidatorTypes ErgoDexDeposit where
    type instance RedeemerType ErgoDexDeposit = PlutusTx.BuiltinData
    type instance DatumType ErgoDexDeposit = DepositConfig

depositInstance :: Scripts.TypedValidator ErgoDexDeposit
depositInstance = Scripts.mkTypedValidator @ErgoDexDeposit
    $$(PlutusTx.compile [|| mkDepositValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @DepositConfig

data ErgoDexSwap
instance Scripts.ValidatorTypes ErgoDexSwap where
    type instance RedeemerType ErgoDexSwap = PlutusTx.BuiltinData
    type instance DatumType ErgoDexSwap = SwapConfig

swapInstance :: Scripts.TypedValidator ErgoDexSwap
swapInstance = Scripts.mkTypedValidator @ErgoDexSwap
    $$(PlutusTx.compile [|| mkSwapValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @SwapConfig

data ErgoDexRedeem
instance Scripts.ValidatorTypes ErgoDexRedeem where
    type instance RedeemerType ErgoDexRedeem = PlutusTx.BuiltinData
    type instance DatumType ErgoDexRedeem = RedeemConfig

redeemInstance :: Scripts.TypedValidator ErgoDexRedeem
redeemInstance = Scripts.mkTypedValidator @ErgoDexRedeem
    $$(PlutusTx.compile [|| mkRedeemValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @RedeemConfig
