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

module ErgoDex.Contracts.OffChain where

import ErgoDex.Contracts.Pool (PoolConfig, PoolAction, mkPoolValidator)

import qualified Ledger.Typed.Scripts as Scripts
import qualified PlutusTx

data ErgoDexPool
instance Scripts.ValidatorTypes ErgoDexPool where
    type instance RedeemerType ErgoDexPool = PoolAction
    type instance DatumType    ErgoDexPool = PoolConfig

poolInstance :: Scripts.TypedValidator ErgoDexPool
poolInstance = Scripts.mkTypedValidator @ErgoDexPool
    $$(PlutusTx.compile [|| mkPoolValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @PoolConfig @PoolAction
