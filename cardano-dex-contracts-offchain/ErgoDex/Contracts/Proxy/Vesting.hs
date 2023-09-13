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
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}

module ErgoDex.Contracts.Proxy.Vesting where

import qualified Prelude as Haskell

import qualified GHC.Generics as GHC
import Plutus.V1.Ledger.Api (PubKeyHash, POSIXTime)
import Plutus.V1.Ledger.Value
import qualified PlutusTx
import PlutusTx.Prelude

data VestingRedeemer = VestingRedeemer
    { vestingInIx :: Integer
    , rewardOutIx :: Integer
    }
    deriving stock (Haskell.Show, GHC.Generic)

PlutusTx.makeIsDataIndexed ''VestingRedeemer [('VestingRedeemer, 0)]
PlutusTx.makeLift ''VestingRedeemer

data VestingConfig = VestingConfig
    { deadline  :: POSIXTime
    , pkh       :: PubKeyHash
    , vestingAC :: AssetClass 
    }
    deriving stock (Haskell.Show, GHC.Generic)

PlutusTx.makeIsDataIndexed ''VestingConfig [('VestingConfig, 0)]
PlutusTx.makeLift ''VestingConfig