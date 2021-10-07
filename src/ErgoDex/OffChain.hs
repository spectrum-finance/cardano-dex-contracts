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
import qualified Ledger.Typed.Scripts as Scripts
import qualified PlutusTx

data PoolArgs
instance Scripts.ValidatorTypes PoolArgs where
    type instance RedeemerType PoolArgs = PoolAction
    type instance DatumType    PoolArgs = PoolDatum

poolInstance :: Scripts.TypedValidator PoolArgs
poolInstance = Scripts.mkTypedValidator @PoolArgs
    ($$(PlutusTx.compile [|| mkPoolValidator ||]))
     $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @PoolDatum @PoolAction
