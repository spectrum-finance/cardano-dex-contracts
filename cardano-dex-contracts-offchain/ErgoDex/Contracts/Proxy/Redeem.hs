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

module ErgoDex.Contracts.Proxy.Redeem where

import qualified Prelude as Haskell

import Data.Aeson (FromJSON, ToJSON)
import qualified GHC.Generics as GHC
import Plutus.V1.Ledger.Api (PubKeyHash)
import Plutus.V1.Ledger.Value (AssetClass)
import qualified PlutusTx
import PlutusTx.Prelude

data RedeemConfig = RedeemConfig
    { poolNft :: AssetClass
    , poolX :: AssetClass
    , poolY :: AssetClass
    , poolLp :: AssetClass
    , exFee :: Integer
    , rewardPkh :: PubKeyHash
    , stakePkh :: Maybe PubKeyHash
    }
    deriving stock (Haskell.Show, GHC.Generic)
    deriving (FromJSON, ToJSON)
PlutusTx.makeIsDataIndexed ''RedeemConfig [('RedeemConfig, 0)]
PlutusTx.makeLift ''RedeemConfig
