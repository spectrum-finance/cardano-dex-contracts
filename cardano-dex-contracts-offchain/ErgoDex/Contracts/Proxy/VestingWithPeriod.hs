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

module ErgoDex.Contracts.Proxy.VestingWithPeriod where

import qualified Prelude as Haskell

import qualified GHC.Generics as GHC
import Plutus.V1.Ledger.Api (PubKeyHash, POSIXTime)
import Plutus.V1.Ledger.Value
import qualified PlutusTx
import PlutusTx.Prelude

data VestingWithPeriodRedeemer = VestingWithPeriodRedeemer
    { vestingInIx      :: Integer
    , vestingPeriodIdx :: Integer
    }
    deriving stock (Haskell.Show, GHC.Generic)

PlutusTx.makeIsDataIndexed ''VestingWithPeriodRedeemer [('VestingWithPeriodRedeemer, 0)]
PlutusTx.makeLift ''VestingWithPeriodRedeemer

data VestingWithPeriodConfig = VestingWithPeriodConfig
    { vestingStart          :: POSIXTime
    , vestingPeriodDuration :: POSIXTime
    , totalVested           :: Integer
    , periodVested          :: Integer
    , pkhs                  :: [PubKeyHash]
    , vestingAC             :: AssetClass 
    }
    deriving stock (Haskell.Show, GHC.Generic)

PlutusTx.makeIsDataIndexed ''VestingWithPeriodConfig [('VestingWithPeriodConfig, 0)]
PlutusTx.makeLift ''VestingWithPeriodConfig