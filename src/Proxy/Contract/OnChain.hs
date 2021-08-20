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

import           Ledger
import           Ledger.Value
    ( AssetClass (..),
      symbols,
      assetClassValueOf,
      tokenName,
      currencySymbol,
      assetClass )
import           Ledger.Contexts        (ScriptContext(..))
import qualified Ledger.Typed.Scripts   as Scripts
import Plutus.Contract
    ( endpoint,
      utxoAt,
      submitTxConstraints,
      submitTxConstraintsSpending,
      collectFromScript,
      select,
      type (.\/),
      Endpoint,
      Contract,
      AsContractError
    )
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import           Plutus.Contract.Schema ()
import qualified PlutusTx
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
import           Utils
import           PlutusTx.Builtins  (divideInteger, multiplyInteger, addInteger, subtractInteger, lessThanEqInteger)
import           Proxy.Contract.Models
import           Dex.Contract.OnChain

{-# INLINABLE checkCorrectSwap #-}
checkCorrectSwap :: ProxyDatum -> ScriptContext -> Bool
checkCorrectSwap ProxyDatum{..} sCtx =
    traceIfFalse "Swap should satisfy conditions" checkConditions PlutusTx.Prelude.&&
    traceIfFalse "Inputs qty check failed" (check2inputs sCtx) PlutusTx.Prelude.&&
    traceIfFalse "Outputs qty check failed" (check2outputs sCtx)
  where

    ownInput :: TxInInfo
    ownInput = findOwnInput' sCtx

    poolInput :: TxOut
    poolInput = inputLockedByDex' sCtx

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
          relaxedOutput = quoteAmount `addInteger` 1
          fairPrice = (1 `multiplyInteger` baseAmount `multiplyInteger` feeNum) `lessThanEqInteger` (relaxedOutput `multiplyInteger` (poolY `multiplyInteger` feeDenom `addInteger` baseAmount `multiplyInteger` feeNum))
        in fairPrice

{-# INLINABLE checkCorrectDeposit #-}
checkCorrectDeposit :: ProxyDatum -> ScriptContext -> Bool
checkCorrectDeposit ProxyDatum{..} sCtx =
    traceIfFalse "Deposit should satisfy conditions" checkConditions PlutusTx.Prelude.&&
    traceIfFalse "Inputs qty check failed" (check2inputs sCtx) PlutusTx.Prelude.&&
    traceIfFalse "Outputs qty check failed" (check2outputs sCtx)
  where
    ownInput :: TxInInfo
    ownInput = findOwnInput' sCtx

    poolInput :: TxOut
    poolInput = inputLockedByDex' sCtx

    poolInputValue :: Value
    poolInputValue = txOutValue $ poolInput

    previousValue :: Value
    previousValue = txOutValue $ txInInfoResolved ownInput

    newValue :: Value
    newValue = txOutValue $ ownOutput sCtx

    checkConditions :: Bool
    checkConditions =
        let
          supplyLP = lpSupply `subtractInteger` (assetClassValueOf poolInputValue lpProxyToken)
          selfX = assetClassValueOf previousValue xProxyToken
          selfY = assetClassValueOf previousValue yProxyToken
          reservesX = assetClassValueOf poolInputValue xProxyToken
          reservesY = assetClassValueOf poolInputValue yProxyToken
          minimalReward = min (selfX `multiplyInteger` supplyLP `divideInteger` reservesX) (selfY `multiplyInteger` supplyLP `divideInteger` reservesY)
          rewardLP = assetClassValueOf newValue lpProxyToken
        in rewardLP >= minimalReward

{-# INLINABLE checkCorrectRedeem #-}
checkCorrectRedeem :: ProxyDatum -> ScriptContext -> Bool
checkCorrectRedeem ProxyDatum{..} sCtx =
  checkTxConstraint (sCtx) (Constraints.MustBeSignedBy (PubKeyHash userPubKeyHash)) PlutusTx.Prelude.&&
  traceIfFalse "Redeem conditions check failed" isRedeemCorrect PlutusTx.Prelude.&&
  traceIfFalse "Inputs qty check failed" (check2inputs sCtx) PlutusTx.Prelude.&&
  traceIfFalse "Outputs qty check failed" (check2outputs sCtx)
  where

    ownInput :: TxInInfo
    ownInput = findOwnInput' sCtx

    poolInput :: TxOut
    poolInput = inputLockedByDex' sCtx

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
        supplyLP = lpSupply `subtractInteger` (assetClassValueOf poolInputValue lpProxyToken)
        reservesX = assetClassValueOf poolInputValue xProxyToken
        reservesY = assetClassValueOf poolInputValue yProxyToken
        minReturnX = selfLP `multiplyInteger` reservesX `divideInteger` supplyLP
        minReturnY = selfLP `multiplyInteger` reservesY `divideInteger` supplyLP
        returnX = assetClassValueOf newValue xProxyToken
        returnY = assetClassValueOf newValue yProxyToken
      in returnX >= minReturnX && returnY >= minReturnY

{-# INLINABLE mkProxyValidator #-}
mkProxyValidator :: ProxyDatum -> ProxyAction -> ScriptContext -> Bool
mkProxyValidator proxyDatum Swap sCtx   = checkCorrectSwap proxyDatum sCtx
--mkProxyValidator proxyDatum Redeem sCtx = checkCorrectRedeem proxyDatum sCtx
--mkProxyValidator proxyDatum Deposit sCtx = checkCorrectDeposit proxyDatum sCtx
mkProxyValidator _ _ _ = False

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