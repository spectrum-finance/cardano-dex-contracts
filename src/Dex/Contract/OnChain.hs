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


module Dex.Contract.OnChain where

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

import Dex.Types
import Utils

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
checkTokenSwap ErgoDexPool{..} sCtx =
    traceIfFalse "Expected A or B coin to be present in input" inputContainsAOrB &&
    traceIfFalse "Expected correct value of A and B in pool output" correctValueSwap
  where

    coinA :: Coin CoinA
    coinA =
      let
        tokenNameA = tokenName aTokenName
        currencySymbolA = currencySymbol aCurSymbol
        assetClassA = assetClass currencySymbolA tokenNameA
      in Coin (assetClassA)

    coinB :: Coin CoinB
    coinB =
      let
        tokenNameB = tokenName bTokenName
        currencySymbolB = currencySymbol bCurSymbol
        assetClassB = assetClass currencySymbolB tokenNameB
      in Coin (assetClassB)

    coinLP :: Coin LPToken
    coinLP =
      let
        tokenNameLP = tokenName lpTokenName
        currencySymbolLP = currencySymbol lpCurSymbol
        assetClassLP = assetClass currencySymbolLP tokenNameLP
      in Coin (assetClassLP)

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
      in calculateValueInOutputs proxyInputs coinB

    proxyInputsWithA :: Integer
    proxyInputsWithA =
      let
        proxyInputs = inputsLockedByDatumHash proxyDatumHash sCtx
      in calculateValueInOutputs proxyInputs coinA

    inputContainsAOrB :: Bool
    inputContainsAOrB =
      let
        input = valueWithin ownInput
        containsA = isUnity input coinB
        containsB = isUnity input coinA
      in containsA || containsB

    correctValueSwap :: Bool
    correctValueSwap =
      let
        outputWithValueToSwap = txInInfoResolved ownInput
        isASwap = isUnity (txOutValue outputWithValueToSwap) coinA
        currentBValue = outputAmountOf currentPoolOutput coinB
        currentAValue = outputAmountOf currentPoolOutput coinA
        currentLpValue = outputAmountOf currentPoolOutput coinLP
        newBValue = outputAmountOf newOutputWithPoolContract coinB
        newAValue = outputAmountOf newOutputWithPoolContract coinA
        newLpToken = outputAmountOf newOutputWithPoolContract coinLP
        correctnewBValue = if isASwap then currentBValue - adaRate proxyInputsWithB else currentBValue + proxyInputsWithB
        correctnewAValue = if isASwap then currentAValue + proxyInputsWithA else currentAValue - ergoRate proxyInputsWithA
      in
        newAValue == correctnewAValue && newBValue == correctnewBValue && currentLpValue == newLpToken

    -- formula from https://github.com/ergoplatform/eips/blob/eip14/eip-0014.md#simple-swap-proxy-contract

    ergoRate :: Integer -> Integer
    ergoRate adaValueToSwap =
      let
        ergoReserved = outputAmountOf currentPoolOutput coinA
        adaReserved = outputAmountOf currentPoolOutput coinB
      in ergoReserved * adaValueToSwap * feeNum `div` (adaReserved * 1000 + adaValueToSwap * feeNum)

    adaRate :: Integer -> Integer
    adaRate ergoValueToSwap =
      let
        ergoReserved = outputAmountOf currentPoolOutput coinA
        adaReserved = outputAmountOf currentPoolOutput coinB
      in adaReserved * ergoValueToSwap * feeNum `div` (ergoReserved * 1000 + ergoValueToSwap * feeNum)

    getTrue :: Bool
    getTrue = True

{-# INLINABLE checkCorrectPoolBootstrapping #-}
checkCorrectPoolBootstrapping :: ErgoDexPool -> ScriptContext -> Bool
checkCorrectPoolBootstrapping ErgoDexPool{..} sCtx =
  traceIfFalse "Incorrect conditions of lp token" lpTokenCond &&
  traceIfFalse "A and B should be in ouptut" isAAndBCoinExists
  where

    coinA :: Coin CoinA
    coinA =
      let
        tokenNameA = tokenName aTokenName
        currencySymbolA = currencySymbol aCurSymbol
        assetClassA = assetClass currencySymbolA tokenNameA
      in Coin (assetClassA)

    coinB :: Coin CoinB
    coinB =
      let
        tokenNameB = tokenName bTokenName
        currencySymbolB = currencySymbol bCurSymbol
        assetClassB = assetClass currencySymbolB tokenNameB
      in Coin (assetClassB)

    coinLP :: Coin LPToken
    coinLP =
      let
        tokenNameLP = tokenName lpTokenName
        currencySymbolLP = currencySymbol lpCurSymbol
        assetClassLP = assetClass currencySymbolLP tokenNameLP
      in Coin (assetClassLP)

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
       lpTokenExsit = isUnity (txOutValue newOutputWithPoolContract) coinLP
       lpTokenAmount = outputAmountOf newOutputWithPoolContract coinLP
       adaAmount = outputAmountOf newOutputWithPoolContract coinB
       ergoAmount = outputAmountOf newOutputWithPoolContract coinA
       correctLpValue = adaAmount * ergoAmount
      in
        lpTokenExsit && lpTokenAmount * lpTokenAmount >= correctLpValue --check

    isAAndBCoinExists :: Bool
    isAAndBCoinExists =
      let
        isAExists = isUnity (txOutValue newOutputWithPoolContract) coinA
        isBExists = isUnity (txOutValue newOutputWithPoolContract) coinB
        adaAmount = outputAmountOf newOutputWithPoolContract coinB
        ergoAmount = outputAmountOf newOutputWithPoolContract coinA
      in
        isAExists && isBExists && adaAmount > 0 && ergoAmount > 0

{-# INLINABLE checkCorrectDepositing #-}
checkCorrectDepositing :: ErgoDexPool -> ScriptContext -> Bool
checkCorrectDepositing ErgoDexPool{..} sCtx =
  traceIfFalse "Incorrect lp token value" checkLpTokenSwap
  where

    coinA :: Coin CoinA
    coinA =
      let
        tokenNameA = tokenName aTokenName
        currencySymbolA = currencySymbol aCurSymbol
        assetClassA = assetClass currencySymbolA tokenNameA
      in Coin (assetClassA)

    coinB :: Coin CoinB
    coinB =
      let
        tokenNameB = tokenName bTokenName
        currencySymbolB = currencySymbol bCurSymbol
        assetClassB = assetClass currencySymbolB tokenNameB
      in Coin (assetClassB)

    coinLP :: Coin LPToken
    coinLP =
      let
        tokenNameLP = tokenName lpTokenName
        currencySymbolLP = currencySymbol lpCurSymbol
        assetClassLP = assetClass currencySymbolLP tokenNameLP
      in Coin (assetClassLP)

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
        ergoValueToDeposit = outputAmountOf outputToSpent coinA
        adaValueToDeposit = outputAmountOf outputToSpent coinB
        currentAReserved = outputAmountOf currentPoolOutput coinA
        currentBReserved = outputAmountOf currentPoolOutput coinB
        currentLpReserved = outputAmountOf currentPoolOutput coinLP
        newAValue = outputAmountOf newOutputWithPoolContract coinA
        newBValue = outputAmountOf newOutputWithPoolContract coinB
        prevLpValue = outputAmountOf currentPoolOutput coinLP
        newLpDecValue = outputAmountOf newOutputWithPoolContract coinLP
        correctLpRew = min (ergoValueToDeposit * lpSupply `div` currentAReserved) (adaValueToDeposit * lpSupply `div` currentBReserved)
      in
        newAValue == currentAReserved + ergoValueToDeposit &&
        newBValue == currentBReserved + adaValueToDeposit &&
        newLpDecValue == currentLpReserved - correctLpRew

{-# INLINABLE checkCorrectRedemption #-}
checkCorrectRedemption :: ErgoDexPool -> ScriptContext -> Bool
checkCorrectRedemption ErgoDexPool{..} sCtx =
  traceIfFalse "Incorrect lp token value" True
  where

    coinA :: Coin CoinA
    coinA =
      let
        tokenNameA = tokenName aTokenName
        currencySymbolA = currencySymbol aCurSymbol
        assetClassA = assetClass currencySymbolA tokenNameA
      in Coin (assetClassA)

    coinB :: Coin CoinB
    coinB =
      let
        tokenNameB = tokenName bTokenName
        currencySymbolB = currencySymbol bCurSymbol
        assetClassB = assetClass currencySymbolB tokenNameB
      in Coin (assetClassB)

    coinLP :: Coin LPToken
    coinLP =
      let
        tokenNameLP = tokenName lpTokenName
        currencySymbolLP = currencySymbol lpCurSymbol
        assetClassLP = assetClass currencySymbolLP tokenNameLP
      in Coin (assetClassLP)

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
        lpRet = outputAmountOf outputToSpent coinLP
        currentAReserved = outputAmountOf currentPoolOutput coinA
        currentBReserved = outputAmountOf currentPoolOutput coinB
        currentLpReserved = outputAmountOf currentPoolOutput coinLP
        newAValue = outputAmountOf newOutputWithPoolContract coinA
        newBValue = outputAmountOf newOutputWithPoolContract coinB
        prevLpValue = outputAmountOf currentPoolOutput coinLP
        newLpDecValue = outputAmountOf newOutputWithPoolContract coinLP
        correctARew = lpRet * currentAReserved `div` lpSupply
        correctBRew =  lpRet * currentBReserved `div` lpSupply
      in
        newAValue == currentAReserved - correctARew &&
        newBValue == currentBReserved - correctBRew &&
        newLpDecValue == currentLpReserved + lpRet

{-# INLINABLE mkDexValidator #-}
mkDexValidator :: ErgoDexPool -> ContractAction -> ScriptContext -> Bool
mkDexValidator pool Create sCtx    = checkCorrectPoolBootstrapping pool sCtx
mkDexValidator _ _ _ = False
-- mkDexValidator pool SwapLP sCtx    = checkCorrectRedemption pool sCtx
-- mkDexValidator pool AddTokens sCtx = checkCorrectDepositing pool sCtx
-- mkDexValidator pool SwapToken sCtx = checkTokenSwap pool sCtx