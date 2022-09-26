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

module ErgoDex.Contracts.Proxy.Order (
    OrderRedeemer (..),
    OrderAction (..),
    isAda,
) where

import qualified Prelude as Haskell

import Plutus.V1.Ledger.Value
import qualified PlutusTx
import PlutusTx.Builtins
import PlutusTx.IsData.Class
import PlutusTx.Prelude

import ErgoDex.Plutus (adaAssetClass)

data OrderAction = Apply | Refund
    deriving (Haskell.Show)
PlutusTx.makeLift ''OrderAction

instance FromData OrderAction where
    {-# INLINE fromBuiltinData #-}
    fromBuiltinData d = matchData' d (\_ _ -> Nothing) (const Nothing) (const Nothing) chooseAction (const Nothing)
      where
        chooseAction i
            | i == 0 = Just Apply
            | i == 1 = Just Refund
            | otherwise = Nothing

instance UnsafeFromData OrderAction where
    {-# INLINE unsafeFromBuiltinData #-}
    unsafeFromBuiltinData = maybe (Haskell.error "Couldn't convert OrderAction from builtin data") id . fromBuiltinData

instance ToData OrderAction where
    {-# INLINE toBuiltinData #-}
    toBuiltinData a = mkI $ case a of
        Apply -> 0
        Refund -> 1

data OrderRedeemer = OrderRedeemer
    { poolInIx :: Integer
    , orderInIx :: Integer
    , rewardOutIx :: Integer
    , action :: OrderAction
    }
    deriving stock (Haskell.Show)
PlutusTx.makeIsDataIndexed ''OrderRedeemer [('OrderRedeemer, 0)]
PlutusTx.makeLift ''OrderRedeemer

isAda :: AssetClass -> Bool
isAda cls = cls == adaAssetClass
