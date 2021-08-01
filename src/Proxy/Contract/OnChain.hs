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

    poolInput :: TxOut
    poolInput = PList.head $ inputsLockedByDatumHash dexContractHash sCtx

    poolInputValue :: Value
    poolInputValue = txOutValue $ poolInput

    previousValue :: Value
    previousValue = txOutValue $ txInInfoResolved ownInput

    newValue :: Value
    newValue = txOutValue $ ownOutput sCtx

    checkConditions :: Bool
    checkConditions =
        let
          outputWithValueToSwap = txInInfoResolved ownInput
          baseAmount = assetClassValueOf previousValue xProxyToken
          quoteAmount = assetClassValueOf newValue yProxyToken
          poolX = assetClassValueOf poolInputValue xProxyToken
          poolY = assetClassValueOf poolInputValue yProxyToken
          relaxedOutput = quoteAmount + 1
          fairPrice = poolX * baseAmount * feeNum <= relaxedOutput * (poolY * feeDenom + baseAmount * feeNum)
        in fairPrice

{-# INLINABLE checkCorrectDeposit #-}
checkCorrectDeposit :: ProxyDatum -> ScriptContext -> Bool
checkCorrectDeposit ProxyDatum{..} sCtx =
    traceIfFalse "Deposit should satisfy conditions" True
  where
    ownInput :: TxInInfo
    ownInput = findOwnInput' sCtx

    poolInput :: TxOut
    poolInput = PList.head $ inputsLockedByDatumHash dexContractHash sCtx

    poolInputValue :: Value
    poolInputValue = txOutValue $ poolInput

    previousValue :: Value
    previousValue = txOutValue $ txInInfoResolved ownInput

    newValue :: Value
    newValue = txOutValue $ ownOutput sCtx

    checkConditions :: Bool
    checkConditions =
        let
          supplyLP = lpSupply - (assetClassValueOf poolInputValue lpProxyToken)
          selfX = assetClassValueOf previousValue xProxyToken
          selfY = assetClassValueOf previousValue yProxyToken
          reservesX = assetClassValueOf poolInputValue xProxyToken
          reservesY = assetClassValueOf poolInputValue yProxyToken
          minimalReward = min (selfX * supplyLP / reservesX) (selfY * supplyLP / reservesY)
          rewardLP = assetClassValueOf newValue lpProxyToken
        in rewardLP >= minimalReward

{-# INLINABLE checkCorrectReturn #-}
checkCorrectRedeem :: ProxyDatum -> ScriptContext -> Bool
checkCorrectRedeem ProxyDatum{..} sCtx =
  checkTxConstraint (sCtx) (Constraints.MustBeSignedBy (PubKeyHash userPubKeyHash)) PlutusTx.Prelude.&&
  traceIfFalse "Recepient should be issuer" isRedeemCorrect
  where

    ownInput :: TxInInfo
    ownInput = findOwnInput' sCtx

    poolInput :: TxOut
    poolInput = PList.head $ inputsLockedByDatumHash dexContractHash sCtx

    poolInputValue :: Value
    poolInputValue = txOutValue $ poolInput

    previousValue :: Value
    previousValue = txOutValue $ txInInfoResolved ownInput

    newValue :: Value
    newValue = txOutValue $ ownOutput sCtx

    isRedeemCorrect :: Bool
    isRedeemCorrect =
      let
        selfLP = assetClassValueOf previousValue lpProxyToken
        supplyLP = lpSupply - (assetClassValueOf poolInputValue lpProxyToken)
        reservesX = assetClassValueOf poolInputValue xProxyToken
        reservesY = assetClassValueOf poolInputValue yProxyToken
        minReturnX = selfLP * reservesX / supplyLP
        minReturnY = selfLP * reservesY / supplyLP
        returnX = assetClassValueOf newValue xProxyToken
        returnY = assetClassValueOf newValue yProxyToken
      in returnX >= minReturnX && returnY >= minReturnY

{-# INLINABLE mkProxyValidator #-}
mkProxyValidator :: ProxyDatum -> ProxyAction -> ScriptContext -> Bool
mkProxyValidator proxyDatum Swap sCtx   = checkCorrectSwap proxyDatum sCtx
mkProxyValidator proxyDatum Redeem sCtx = checkCorrectRedeem proxyDatum sCtx
mkProxyValidator proxyDatum Deposit sCtx = checkCorrectDeposit proxyDatum sCtx

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

feeNum :: Integer
feeNum = 995

feeDenom :: Integer
feeDenom = 1000