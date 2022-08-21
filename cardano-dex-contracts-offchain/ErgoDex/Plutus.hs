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

import qualified Plutus.V1.Ledger.Value as Value
import Plutus.V1.Ledger.Contexts
import Plutus.V1.Ledger.Value
import PlutusTx.Prelude

adaAssetClass :: AssetClass
adaAssetClass = assetClass Value.adaSymbol Value.adaToken

valueWithin :: TxInInfo -> Value
valueWithin = txOutValue . txInInfoResolved

inputsNum :: ScriptContext -> Integer
inputsNum sCtx = length $ txInfoInputs $ scriptContextTxInfo sCtx