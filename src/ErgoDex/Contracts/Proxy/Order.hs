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

module ErgoDex.Contracts.Proxy.Order where

import           Ledger
import           ErgoDex.Contracts.Types
import           PlutusTx.Prelude
import           ErgoDex.Plutus   (adaAssetClass)

{-# INLINABLE getOrderInput #-}
getOrderInput :: ScriptContext -> TxOut
getOrderInput ScriptContext{scriptContextTxInfo=TxInfo{txInfoInputs}} =
  txInInfoResolved $ txInfoInputs !! 1 -- order box is always 2nd input

{-# INLINABLE getOrderRewardOutput #-}
getOrderRewardOutput :: ScriptContext -> TxOut
getOrderRewardOutput ScriptContext{scriptContextTxInfo=TxInfo{txInfoOutputs}} =
  txInfoOutputs !! 1 -- order reward box is always 2nd output

{-# INLINABLE isAda #-}
isAda :: Coin a -> Bool
isAda (Coin cls) = cls == adaAssetClass
