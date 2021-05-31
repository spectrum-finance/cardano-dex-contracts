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


module ErgoDex.OnChain (checkTokenSwap) where

import           Control.Monad          (void)
import           GHC.Generics           (Generic)
import           Ledger.Value           (AssetClass (..), symbols, assetClassValueOf)
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
import qualified Ledger.Ada             as Ada

import qualified Prelude
import           Schema                 (ToArgument, ToSchema)
import           Wallet.Emulator        (Wallet (..))

import ErgoDex.Types
import qualified Prelude

--todo: Refactoring. Check that value of ergo, ada is greather than 0. validate creation, adding ada/ergo to

{-# INLINABLE findOwnInput' #-}
findOwnInput' :: ScriptContext -> TxInInfo
findOwnInput' ctx = fromMaybe (error ()) (findOwnInput ctx)

{-# INLINABLE valueWithin #-}
valueWithin :: TxInInfo -> Value
valueWithin = txOutValue . txInInfoResolved

{-# INLINABLE feeNum #-}
feeNum :: Integer
feeNum = 997

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
    foldl (getAmountAndSum) (0 :: Integer) outputs
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
                                    , txOutDatumHash (txInInfoResolved proxyInput) == Just (hash)
                                    ]

{-# INLINABLE checkTokenSwap #-}
checkTokenSwap :: ErgoDexPool -> Redeemer -> ScriptContext -> Bool
checkTokenSwap ErgoDexPool{..} _ sCtx =
    traceIfFalse "Expected Ergo or Ada coin to be present in input" inputContainsErgoOrAda &&
    traceIfFalse "Expected correct value of Ergo and Ada in pool output" correctValueSwap &&
    traceIfFalse "Value of LP Token should't change" Bool
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

    proxyInputsWithAda :: Integer
    proxyInputsWithAda =
      let
        proxyInputs = inputsLockedByDatumHash proxyDatumHash sCtx
      in calculateValueInOutputs proxyInputs adaCoin

    proxyInputsWithErgo :: Integer
    proxyInputsWithErgo =
      let
        proxyInputs = inputsLockedByDatumHash proxyDatumHash sCtx
      in calculateValueInOutputs proxyInputs ergoCoin

    inputContainsErgoOrAda :: Bool
    inputContainsErgoOrAda =
      let
        input = valueWithin ownInput
        containsErgo = isUnity input adaCoin
        containsAda = isUnity input ergoCoin
      in containsErgo || containsAda

    correctValueSwap :: Bool
    correctValueSwap =
      let
        outputWithValueToSwap = txInInfoResolved ownInput
        isErgoSwap = isUnity (txOutValue outputWithValueToSwap) ergoCoin
        currentAdaValue = assetClassValueOf (txOutValue currentPoolOutput) (unCoin adaCoin)
        currentErgoValue = assetClassValueOf (txOutValue currentPoolOutput) (unCoin ergoCoin)
        newAdaValue = assetClassValueOf (txOutValue newOutputWithPoolContract) (unCoin adaCoin)
        newErgoValue = assetClassValueOf (txOutValue newOutputWithPoolContract) (unCoin ergoCoin)
        correctNewAdaValue = if isErgoSwap then currentAdaValue - adaRate proxyInputsWithAda else currentAdaValue + proxyInputsWithAda
        correctNewErgoValue = if isErgoSwap then currentErgoValue + proxyInputsWithErgo else currentErgoValue - ergoRate proxyInputsWithErgo
      in
        (newErgoValue == correctNewErgoValue) && (newAdaValue == correctNewAdaValue)

    -- formula from https://github.com/ergoplatform/eips/blob/eip14/eip-0014.md#simple-swap-proxy-contract



    ergoRate :: Integer -> Integer
    ergoRate adaValueToSwap =
      let
        currentPoolValue = txOutValue currentPoolOutput
        ergoReserved = unAmount $ amountOf currentPoolValue ergoCoin
        adaReserved = unAmount $ amountOf currentPoolValue adaCoin
      in ergoReserved * adaValueToSwap * feeNum `div` (adaReserved * 1000 + adaValueToSwap * feeNum)

    adaRate :: Integer -> Integer
    adaRate ergoValueToSwap =
      let
        currentPoolValue = txOutValue currentPoolOutput
        ergoReserved = unAmount $ amountOf currentPoolValue ergoCoin
        adaReserved = unAmount $ amountOf currentPoolValue adaCoin
      in adaReserved * ergoValueToSwap * feeNum `div` (ergoReserved * 1000 + ergoValueToSwap * feeNum)

    getTrue :: Bool
    getTrue = True

{-# INLINABLE checkCorrectPoolBootstrapping #-}
checkCorrectPoolBootstrapping :: ErgoDexPool -> Redeemer -> ScriptContext -> Bool
checkCorrectPoolBootstrapping ErgoDexPool{..} _ sCtx =
  traceIfFalse "Incorrect conditions of lp token" lpTokenCond &&
  traceIfFalse "Ergo and Ada should be in ouptut" isErgoAndAdaCoinExists
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
       lpTokenExsit = isUnity (txOutValue newOutputWithPoolContract) lpToken
       lpTokenAmount = amountOf (txOutValue newOutputWithPoolContract) lpToken
       adaAmount = unAmount (amountOf (txOutValue newOutputWithPoolContract) adaCoin)
       ergoAmount = unAmount (amountOf (txOutValue newOutputWithPoolContract) ergoCoin)
      in
        lpTokenExsit && lpTokenAmount == sqrt (fromIntegral $ adaAmount * ergoAmount)

    isErgoAndAdaCoinExists :: Bool
    isErgoAndAdaCoinExists =
      let
        isErgoExists = isUnity (txOutValue newOutputWithPoolContract) ergoCoin
        isAdaExists = isUnity (txOutValue newOutputWithPoolContract) adaCoin
        adaAmount = amountOf (txOutValue newOutputWithPoolContract) adaCoin
        ergoAmount = amountOf (txOutValue newOutputWithPoolContract) ergoCoin
      in
        isErgoExists && isAdaExists && adaAmount > 0 && ergoAmount > 0

{-# INLINABLE checkCorrectDepositing #-}
checkCorrectDepositing :: ErgoDexPool -> Redeemer -> ScriptContext -> Bool
checkCorrectDepositing ErgoDexPool{..} _ sCtx =
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
        outputToSpent = txInInfoResolved $ findOwnInput' sCtx
        ergoValueToDeposit = unAmount $ amountOf (txOutValue outputToSpent) ergoCoin
        adaValueToDeposit = unAmount $ amountOf (txOutValue outputToSpent) adaCoin
        currentErgoReserved = unAmount $ amountOf (txOutValue currentPoolOutput) ergoCoin
        currentAdaReserved = unAmount $ amountOf (txOutValue currentPoolOutput) adaCoin
        currentLpReserved = unAmount $ amountOf (txOutValue currentPoolOutput) lpToken
        newErgoValue = unAmount $ amountOf (txOutValue newOutputWithPoolContract) ergoCoin
        newAdaValue = unAmount $ amountOf (txOutValue newOutputWithPoolContract) adaCoin
        prevLpValue = unAmount $ amountOf (txOutValue currentPoolOutput) lpToken
        newLpDecValue = unAmount $ amountOf (txOutValue newOutputWithPoolContract) lpToken
        correctLpRew = min (ergoValueToDeposit * lpSupply `div` currentErgoReserved) (adaValueToDeposit * lpSupply `div` currentAdaReserved)
      in
        newErgoValue == (currentErgoReserved + ergoValueToDeposit) &&
        newAdaValue == (currentAdaReserved + adaValueToDeposit) &&
        newLpDecValue == (currentLpReserved - correctLpRew)

{-# INLINABLE checkCorrectRedemption #-}
checkCorrectRedemption :: ErgoDexPool -> Redeemer -> ScriptContext -> Bool
checkCorrectRedemption ErgoDexPool{..} _ sCtx =
  traceIfFalse "Incorrect lp token value" True
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
        outputToSpent = txInInfoResolved $ findOwnInput' sCtx
        lpRet = unAmount $ amountOf (txOutValue outputToSpent) lpToken
        currentErgoReserved = unAmount $ amountOf (txOutValue currentPoolOutput) ergoCoin
        currentAdaReserved = unAmount $ amountOf (txOutValue currentPoolOutput) adaCoin
        currentLpReserved = unAmount $ amountOf (txOutValue currentPoolOutput) lpToken
        newErgoValue = unAmount $ amountOf (txOutValue newOutputWithPoolContract) ergoCoin
        newAdaValue = unAmount $ amountOf (txOutValue newOutputWithPoolContract) adaCoin
        prevLpValue = unAmount $ amountOf (txOutValue currentPoolOutput) lpToken
        newLpDecValue = unAmount $ amountOf (txOutValue newOutputWithPoolContract) lpToken
        correctErgoRew = (lpRet * currentErgoReserved) `div` lpSupply
        correctAdaRew =  (lpRet * currentAdaReserved) `div` lpSupply
      in
        newErgoValue == (currentErgoReserved - correctErgoRew) &&
        newAdaValue == (currentAdaReserved - correctAdaRew) &&
        newLpDecValue == (currentLpReserved + lpRet)
