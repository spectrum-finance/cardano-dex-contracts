{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# options_ghc -fno-warn-orphans          #-}
{-# options_ghc -Wno-redundant-constraints #-}
{-# options_ghc -fno-strictness            #-}
{-# options_ghc -fno-specialise            #-}

module Dex.Contract.OffChain where

import           Ledger
import           Ledger.Value        (AssetClass (..), assetClass, assetClassValue, assetClassValueOf)
import           Playground.Contract (FromJSON, Generic, ToJSON, ToSchema)
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude                as Haskell
import qualified Ledger.Typed.Scripts   as Scripts
import           Text.Printf         (PrintfArg)
import qualified Data.ByteString.Char8  as C
import Dex.Contract.Models
import Dex.Contract.OnChain

data ErgoDexSwapping
instance Scripts.ValidatorTypes ErgoDexSwapping where
    type instance RedeemerType ErgoDexSwapping = ContractAction
    type instance DatumType    ErgoDexSwapping = ErgoDexPool

dexInstance :: Scripts.TypedValidator ErgoDexSwapping
dexInstance = Scripts.mkTypedValidator @ErgoDexSwapping
    $$(PlutusTx.compile [|| mkDexValidator ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @ErgoDexPool @ContractAction

dexValidator :: Validator
dexValidator = Scripts.validatorScript dexInstance

dexContractHash :: Scripts.ValidatorHash
dexContractHash = Scripts.validatorHash dexValidator