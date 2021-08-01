{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# options_ghc -Wno-redundant-constraints #-}
{-# options_ghc -fno-strictness            #-}
{-# options_ghc -fno-specialise            #-}

module Proxy.Contract.OnChain where

import           Control.Monad          (void)
import           Playground.Contract    (FromJSON, Generic, ToJSON, ToSchema)
import           GHC.Generics           (Generic)
import qualified Data.ByteString        as BS
import           Ledger
import           Ledger.Value
    ( AssetClass (..),
      symbols,
      assetClassValueOf,
      tokenName,
      currencySymbol,
      assetClass )
import           Ledger.Contexts        (ScriptContext(..))
import qualified Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import Plutus.Contract
    ( endpoint,
      utxoAt,
      submitTxConstraints,
      submitTxConstraintsSpending,
      collectFromScript,
      select,
      type (.\/),
      BlockchainActions,
      Endpoint,
      Contract,
      AsContractError
    )
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import           Plutus.Contract.Schema ()
import           Plutus.Trace.Emulator  (EmulatorTrace)
import qualified Plutus.Trace.Emulator  as Trace
import qualified PlutusTx
import qualified Prelude             as Haskell
import           PlutusTx.Prelude
import           PlutusTx.List       as PList
import Ledger
    ( findOwnInput,
      getContinuingOutputs,
      ownHashes,
      pubKeyHashAddress,
      Address,
      ScriptContext(scriptContextTxInfo),
      TxInInfo(txInInfoResolved),
      TxInfo(txInfoInputs),
      DatumHash,
      Redeemer,
      TxOut(txOutDatumHash, txOutValue),
      Value)
import           Schema                 (ToArgument, ToSchema)
import           Wallet.Emulator        (Wallet (..))
import           Utils
import           Proxy.Contract.Models

{-# INLINABLE checkCorrectSwap #-}
checkCorrectSwap :: ProxyDatum -> ScriptContext -> Bool
checkCorrectSwap ProxyDatum{..} sCtx =
    traceIfFalse "Swap should satisfy conditions" True
  where

    ownInput :: TxInInfo
    ownInput = findOwnInput' sCtx

    userOuptut :: TxOut
    userOuptut = PList.head $ inputsLockedByUserPubKeyHash (PubKeyHash userPubKeyHash) sCtx

    isASwap :: Bool
    isASwap =
        let
          outputWithValueToSwap = txInInfoResolved ownInput
          ergoSwapCheck = isUnity (txOutValue outputWithValueToSwap) (Coin xProxyToken)
        in ergoSwapCheck

    checkConditions :: Bool
    checkConditions =
        let
          outputWithValueToSwap = txInInfoResolved ownInput
          inputValue =
              if (isASwap) then outputAmountOf outputWithValueToSwap (Coin xProxyToken) else outputAmountOf outputWithValueToSwap (Coin yProxyToken)
          outputValue =
              if (isASwap) then outputAmountOf userOuptut (Coin yProxyToken) else outputAmountOf userOuptut (Coin xProxyToken)
        in outputValue >= minOutputValue


{-# INLINABLE checkCorrectReturn #-}
checkCorrectReturn :: ProxyDatum -> ScriptContext -> Bool
checkCorrectReturn ProxyDatum{..} sCtx =
  checkTxConstraint (sCtx) (Constraints.MustBeSignedBy (PubKeyHash userPubKeyHash)) PlutusTx.Prelude.&&
  traceIfFalse "Recepient should be issuer" isReturnCorrect
  where

    ownInput :: TxInInfo
    ownInput = findOwnInput' sCtx

    isReturnCorrect :: Bool
    isReturnCorrect =
      let
        value2swap = txOutValue $ txInInfoResolved ownInput
        pubKH = PubKeyHash (userPubKeyHash)
      in checkTxConstraint sCtx (Constraints.MustPayToPubKey pubKH value2swap)

{-# INLINABLE mkProxyValidator #-}
mkProxyValidator :: ProxyDatum -> ProxyAction -> ScriptContext -> Bool
mkProxyValidator proxyDatum Swap sCtx   = checkCorrectSwap proxyDatum sCtx
mkProxyValidator proxyDatum Redeem sCtx = checkCorrectReturn proxyDatum sCtx

data ProxySwapping
instance Scripts.ValidatorTypes ProxySwapping where
    type instance RedeemerType ProxySwapping = ProxyAction
    type instance DatumType    ProxySwapping = ProxyDatum

proxyInstance :: Scripts.TypedValidator ProxySwapping
proxyInstance = Scripts.mkTypedValidator @ProxySwapping
    $$(PlutusTx.compile [|| mkProxyValidator ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @ProxyDatum @ProxyAction

proxyValidator :: Validator
proxyValidator = Scripts.validatorScript proxyInstance