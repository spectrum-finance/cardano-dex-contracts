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

module Proxy.Contract.OnChain where

import           Control.Monad          (void)
import           GHC.Generics           (Generic)
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
      AsContractError,
      ContractError )
import           Plutus.Contract.Schema ()
import           Plutus.Trace.Emulator  (EmulatorTrace)
import qualified Plutus.Trace.Emulator  as Trace
import qualified PlutusTx
import           PlutusTx.Prelude
import Ledger
    ( findOwnInput,
      getContinuingOutputs,
      ownHashes,
      ScriptContext(scriptContextTxInfo),
      TxInInfo(txInInfoResolved),
      TxInfo(txInfoInputs),
      DatumHash,
      Redeemer,
      TxOut(txOutDatumHash, txOutValue),
      Value)
import qualified Ledger.Ada             as Ada

import qualified Prelude
import           Schema                 (ToArgument, ToSchema)
import           Wallet.Emulator        (Wallet (..))

import Dex.Contract.Models
import Utils
    ( amountOf,
      isUnity,
      outputAmountOf,
      Amount(unAmount),
      Coin(Coin),
      CoinA,
      CoinB,
      LPToken,
      getCoinAFromPool,
      getCoinBFromPool,
      getCoinLPFromPool )

--todo: Refactoring. Check that value of ergo, ada is greather than 0. validate creation, adding ada/ergo to

{-# INLINABLE findOwnInput' #-}
findOwnInput' :: ScriptContext -> TxInInfo
findOwnInput' ctx = fromMaybe (error ()) (findOwnInput ctx)

{-# INLINABLE valueWithin #-}
valueWithin :: TxInInfo -> Value
valueWithin = txOutValue . txInInfoResolved

{-# INLINABLE lpSupply #-}
-- todo: set correct lp_supply
lpSupply :: Integer
lpSupply = 4000000000

{-# INLINABLE proxyDatumHash #-}
proxyDatumHash :: DatumHash
proxyDatumHash = datumHashFromString "proxyDatumHash"

{-# INLINABLE calculateValueInOutputs #-}
calculateValueInOutputs :: [TxInInfo] -> Coin a -> Integer
calculateValueInOutputs outputs coinValue =
    foldl getAmountAndSum (0 :: Integer) outputs
  where
    getAmountAndSum :: Integer -> TxInInfo -> Integer
    getAmountAndSum acc out = acc + unAmount (amountOf (txOutValue $ txInInfoResolved out) coinValue)

 -- set correct contract datum hash
{-# INLINABLE currentContractHash #-}
currentContractHash :: DatumHash
currentContractHash = datumHashFromString "dexContractDatumHash"

--refactor
{-# INLINABLE inputsLockedByDatumHash #-}
inputsLockedByDatumHash :: DatumHash -> ScriptContext -> [TxInInfo]
inputsLockedByDatumHash hash sCtx = [ proxyInput
                                    | proxyInput <- txInfoInputs (scriptContextTxInfo sCtx)
                                    , txOutDatumHash (txInInfoResolved proxyInput) == Just hash
                                    ]

{-# INLINABLE checkTokenSwap #-}
checkTokenSwap :: ErgoDexPool -> ScriptContext -> Bool
checkTokenSwap pool sCtx =
    traceIfFalse "Expected A or B coin to be present in input" inputContainsAOrB &&
    traceIfFalse "Expected correct value of A and B in pool output" correctValueSwap
  where

    ownInput :: TxInInfo
    ownInput = findOwnInput' sCtx

    newOutputWithPoolContract :: TxOut
    newOutputWithPoolContract = case [ output
                                     | output <- getContinuingOutputs sCtx
                                     , txOutDatumHash output == Just (snd $ ownHashes sCtx)
                                     ] of
      [output]   -> output
      otherwise  -> traceError "expected exactly one output of ergo dex"

    currentPoolOutput :: TxOut
    currentPoolOutput =
      let
        poolInputs = inputsLockedByDatumHash currentContractHash sCtx
      in
        case poolInputs of
          [input] -> txInInfoResolved input
          otherwise -> traceError "expected exactly one input of ergo dex"

    proxyInputsWithB :: Integer
    proxyInputsWithB =
      let
        proxyInputs = inputsLockedByDatumHash proxyDatumHash sCtx
      in calculateValueInOutputs proxyInputs (getCoinBFromPool pool)

    proxyInputsWithA :: Integer
    proxyInputsWithA =
      let
        proxyInputs = inputsLockedByDatumHash proxyDatumHash sCtx
      in calculateValueInOutputs proxyInputs (getCoinAFromPool pool)

    inputContainsAOrB :: Bool
    inputContainsAOrB =
      let
        input = valueWithin ownInput
        containsA = isUnity input (getCoinBFromPool pool)
        containsB = isUnity input (getCoinAFromPool pool)
      in containsA || containsB

    correctValueSwap :: Bool
    correctValueSwap =
      let
        outputWithValueToSwap = txInInfoResolved ownInput
        isASwap = isUnity (txOutValue outputWithValueToSwap) (getCoinAFromPool pool)
        currentBValue = outputAmountOf currentPoolOutput (getCoinBFromPool pool)
        currentAValue = outputAmountOf currentPoolOutput (getCoinAFromPool pool)
        currentLpValue = outputAmountOf currentPoolOutput (getCoinLPFromPool pool)
        newBValue = outputAmountOf newOutputWithPoolContract (getCoinBFromPool pool)
        newAValue = outputAmountOf newOutputWithPoolContract (getCoinAFromPool pool)
        newLpToken = outputAmountOf newOutputWithPoolContract (getCoinLPFromPool pool)
        correctnewBValue = if isASwap then currentBValue - adaRate proxyInputsWithB else currentBValue + proxyInputsWithB
        correctnewAValue = if isASwap then currentAValue + proxyInputsWithA else currentAValue - ergoRate proxyInputsWithA
      in
        newAValue == correctnewAValue && newBValue == correctnewBValue && currentLpValue == newLpToken

    -- formula from https://github.com/ergoplatform/eips/blob/eip14/eip-0014.md#simple-swap-proxy-contract

    ergoRate :: Integer -> Integer
    ergoRate adaValueToSwap =
      let
        ergoReserved = outputAmountOf currentPoolOutput (getCoinAFromPool pool)
        adaReserved = outputAmountOf currentPoolOutput (getCoinBFromPool pool)
      in ergoReserved * adaValueToSwap * (feeNum pool) `Prelude.div` (adaReserved * 1000 + adaValueToSwap * (feeNum pool))

    adaRate :: Integer -> Integer
    adaRate ergoValueToSwap =
      let
        ergoReserved = outputAmountOf currentPoolOutput (getCoinAFromPool pool)
        adaReserved = outputAmountOf currentPoolOutput (getCoinBFromPool pool)
      in adaReserved * ergoValueToSwap * (feeNum pool) `Prelude.div` (ergoReserved * 1000 + ergoValueToSwap * (feeNum pool))

    getTrue :: Bool
    getTrue = True

{-# INLINABLE checkCorrectPoolBootstrapping #-}
checkCorrectPoolBootstrapping :: ErgoDexPool -> ScriptContext -> Bool
checkCorrectPoolBootstrapping pool sCtx =
  traceIfFalse "Incorrect conditions of lp token" lpTokenCond &&
  traceIfFalse "A and B should be in ouptut" isAAndBCoinExists
  where

    ownInput :: TxInInfo
    ownInput = findOwnInput' sCtx

    newOutputWithPoolContract :: TxOut
    newOutputWithPoolContract = case [ output
                                     | output <- getContinuingOutputs sCtx
                                     , txOutDatumHash output == Just (snd $ ownHashes sCtx)
                                     ] of
      [output]   -> output
      otherwise  -> traceError "expected exactly one output of ergo dex"

    lpTokenCond :: Bool
    lpTokenCond =
      let
       lpTokenExsit = isUnity (txOutValue newOutputWithPoolContract) (getCoinLPFromPool pool)
       lpTokenAmount = outputAmountOf newOutputWithPoolContract (getCoinLPFromPool pool)
       adaAmount = outputAmountOf newOutputWithPoolContract (getCoinBFromPool pool)
       ergoAmount = outputAmountOf newOutputWithPoolContract (getCoinAFromPool pool)
       correctLpValue = adaAmount * ergoAmount
      in
        lpTokenExsit && lpTokenAmount * lpTokenAmount >= correctLpValue --check

    isAAndBCoinExists :: Bool
    isAAndBCoinExists =
      let
        isAExists = isUnity (txOutValue newOutputWithPoolContract) (getCoinAFromPool pool)
        isBExists = isUnity (txOutValue newOutputWithPoolContract) (getCoinBFromPool pool)
        adaAmount = outputAmountOf newOutputWithPoolContract (getCoinBFromPool pool)
        ergoAmount = outputAmountOf newOutputWithPoolContract (getCoinAFromPool pool)
      in
        isAExists && isBExists && adaAmount > 0 && ergoAmount > 0

{-# INLINABLE checkCorrectDepositing #-}
checkCorrectDepositing :: ErgoDexPool -> ScriptContext -> Bool
checkCorrectDepositing pool sCtx =
  traceIfFalse "Incorrect lp token value" checkLpTokenSwap
  where

    newOutputWithPoolContract :: TxOut
    newOutputWithPoolContract = case [ output
                                     | output <- getContinuingOutputs sCtx
                                     , txOutDatumHash output == Just (snd $ ownHashes sCtx)
                                     ] of
      [output]   -> output
      otherwise  -> traceError "expected exactly one output of ergo dex"

    currentPoolOutput :: TxOut
    currentPoolOutput =
      let
        poolInputs = inputsLockedByDatumHash currentContractHash sCtx
      in
        case poolInputs of
          [input] -> txInInfoResolved input
          otherwise -> traceError "expected exactly one input of ergo dex"

    checkLpTokenSwap :: Bool
    checkLpTokenSwap =
      let
        at = True
        outputToSpent = txInInfoResolved $ findOwnInput' sCtx
        ergoValueToDeposit = outputAmountOf outputToSpent (getCoinAFromPool pool)
        adaValueToDeposit = outputAmountOf outputToSpent (getCoinBFromPool pool)
        currentAReserved = outputAmountOf currentPoolOutput (getCoinAFromPool pool)
        currentBReserved = outputAmountOf currentPoolOutput (getCoinBFromPool pool)
        currentLpReserved = outputAmountOf currentPoolOutput (getCoinLPFromPool pool)
        newAValue = outputAmountOf newOutputWithPoolContract (getCoinAFromPool pool)
        newBValue = outputAmountOf newOutputWithPoolContract (getCoinBFromPool pool)
        prevLpValue = outputAmountOf currentPoolOutput (getCoinLPFromPool pool)
        newLpDecValue = outputAmountOf newOutputWithPoolContract (getCoinLPFromPool pool)
        correctLpRew = min (ergoValueToDeposit * lpSupply `Prelude.div` currentAReserved) (adaValueToDeposit * lpSupply `Prelude.div` currentBReserved)
        newAValueCheck = (newAValue == (currentAReserved + ergoValueToDeposit))
        newBValueCheck = newBValue == currentBReserved + adaValueToDeposit
        newLpDecValueCheck = newLpDecValue == currentLpReserved - correctLpRew
      in newAValueCheck && newBValueCheck && newLpDecValueCheck
        -- newBValue == currentBReserved + adaValueToDeposit &&
        -- newLpDecValue == currentLpReserved - correctLpRew

{-# INLINABLE checkCorrectRedemption #-}
checkCorrectRedemption :: ErgoDexPool -> ScriptContext -> Bool
checkCorrectRedemption pool sCtx =
  traceIfFalse "Incorrect lp token value" True
  where

    newOutputWithPoolContract :: TxOut
    newOutputWithPoolContract = case [ output
                                     | output <- getContinuingOutputs sCtx
                                     , txOutDatumHash output == Just (snd $ ownHashes sCtx)
                                     ] of
      [output]   -> output
      otherwise  -> traceError "expected exactly one output of dex"

    currentPoolOutput :: TxOut
    currentPoolOutput =
      let
        poolInputs = inputsLockedByDatumHash currentContractHash sCtx
      in
        case poolInputs of
          [input] -> txInInfoResolved input
          otherwise -> traceError "expected exactly one input of dex"

    checkLpTokenSwap :: Bool
    checkLpTokenSwap =
      let
        outputToSpent = txInInfoResolved $ findOwnInput' sCtx
        lpRet = outputAmountOf outputToSpent (getCoinLPFromPool pool)
        currentAReserved = outputAmountOf currentPoolOutput (getCoinAFromPool pool)
        currentBReserved = outputAmountOf currentPoolOutput (getCoinBFromPool pool)
        currentLpReserved = outputAmountOf currentPoolOutput (getCoinLPFromPool pool)
        newAValue = outputAmountOf newOutputWithPoolContract (getCoinAFromPool pool)
        newBValue = outputAmountOf newOutputWithPoolContract (getCoinBFromPool pool)
        prevLpValue = outputAmountOf currentPoolOutput (getCoinLPFromPool pool)
        newLpDecValue = outputAmountOf newOutputWithPoolContract (getCoinLPFromPool pool)
        correctARew = lpRet * currentAReserved `Prelude.div` lpSupply
        correctBRew =  lpRet * currentBReserved `Prelude.div` lpSupply
      in
        newAValue == currentAReserved - correctARew &&
        newBValue == currentBReserved - correctBRew &&
        newLpDecValue == currentLpReserved + lpRet

{-# INLINABLE mkDexValidator #-}
mkDexValidator :: ErgoDexPool -> ContractAction -> ScriptContext -> Bool
mkDexValidator pool Create sCtx    = checkCorrectPoolBootstrapping pool sCtx
mkDexValidator pool SwapLP sCtx    = checkCorrectRedemption pool sCtx
-- mkDexValidator pool AddTokens sCtx = checkCorrectDepositing pool sCtx
-- mkDexValidator pool SwapToken sCtx = checkTokenSwap pool sCtx
mkDexValidator _ _ _ = False