{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}

module ErgoDex.Plutus where

import qualified Plutus.V1.Ledger.Ada as Ada
import Plutus.V1.Ledger.Contexts
import Plutus.V1.Ledger.Value
import PlutusTx.Prelude

{-# INLINEABLE adaAssetClass #-}
adaAssetClass :: AssetClass
adaAssetClass = assetClass Ada.adaSymbol Ada.adaToken

{-# INLINEABLE valueWithin #-}
valueWithin :: TxInInfo -> Value
valueWithin = txOutValue . txInInfoResolved

{-# INLINEABLE inputsNum #-}
inputsNum :: ScriptContext -> Integer
inputsNum sCtx = length $ txInfoInputs $ scriptContextTxInfo sCtx
