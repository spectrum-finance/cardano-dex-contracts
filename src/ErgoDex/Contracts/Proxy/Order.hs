{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module ErgoDex.Contracts.Proxy.Order
  ( OrderRedeemer(..)
  , isAda
  , findOrderInput
  , findRewardInput
  ) where

import qualified Prelude as Haskell

import           Ledger
import           PlutusTx.Prelude
import qualified PlutusTx

import ErgoDex.Plutus (adaAssetClass)

data OrderRedeemer = OrderRedeemer
  { poolInIx    :: Integer
  , orderInIx   :: Integer
  , rewardOutIx :: Integer
  } deriving stock (Haskell.Show)
PlutusTx.makeIsDataIndexed ''OrderRedeemer [('OrderRedeemer, 0)]
PlutusTx.makeLift ''OrderRedeemer

{-# INLINABLE isAda #-}
isAda :: AssetClass -> Bool
isAda cls = cls == adaAssetClass

{-# INLINABLE findOrderInput #-}
findOrderInput :: ScriptContext -> TxOut
findOrderInput ctx = txInInfoResolved $ fromMaybe (error ()) (findOwnInput ctx)

{-# INLINABLE findRewardInput #-}
findRewardInput :: ScriptContext -> PubKeyHash -> TxOut
findRewardInput ScriptContext{scriptContextTxInfo=TxInfo{txInfoInputs}} pkh =
  txInInfoResolved $ fromMaybe (error ()) (find isReward txInfoInputs)
    where isReward TxInInfo{txInInfoResolved} = maybe False (== pkh) (pubKeyOutput txInInfoResolved)
