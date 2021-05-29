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

findOwnInput' :: ScriptContext -> TxInInfo
findOwnInput' ctx = fromMaybe (error ()) (findOwnInput ctx)

valueWithin :: TxInInfo -> Value
valueWithin = txOutValue . txInInfoResolved

proxyDatumHash :: DatumHash
proxyDatumHash = datumHashFromString "proxyDatumHash"

calculateValueInOutputs :: [TxInInfo] -> Coin a -> Integer
calculateValueInOutputs outputs coinValue =
    foldl (getAmountAndSum) (0 :: Integer) outputs
  where
    getAmountAndSum :: Integer -> TxInInfo -> Integer
    getAmountAndSum acc out = acc + unAmount (amountOf (txOutValue $ txInInfoResolved out) coinValue)

 -- set correct contract datum hash
currentContractHash :: DatumHash
currentContractHash = datumHashFromString "dexContractDatumHash"

--refactor

inputsLockedByDatumHash :: DatumHash -> ScriptContext -> [TxInInfo]
inputsLockedByDatumHash hash sCtx = [ proxyInput
                                    | proxyInput <- txInfoInputs (scriptContextTxInfo sCtx)
                                    , txOutDatumHash (txInInfoResolved proxyInput) == Just (hash)
                                    ]

checkTokenSwap :: ErgoDexPool -> Redeemer -> ScriptContext -> Bool
checkTokenSwap ErgoDexPool{..} _ sCtx =
    traceIfFalse "Expected Ergo or Ada coin to be present in input" inputContainsErgoOrAda &&
    traceIfFalse "Expected correct value of Ergo and Ada in pool output" correctValueSwap
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
        currentAdaValue = assetClassValueOf (txOutValue currentPoolOutput) (unCoin adaCoin)
        currentErgoValue = assetClassValueOf (txOutValue currentPoolOutput) (unCoin ergoCoin)
        newAdaValue = assetClassValueOf (txOutValue newOutputWithPoolContract) (unCoin adaCoin)
        newErgoValue = assetClassValueOf (txOutValue newOutputWithPoolContract) (unCoin ergoCoin)
      in
        (newErgoValue == currentErgoValue + proxyInputsWithErgo) && (newAdaValue == currentAdaValue + proxyInputsWithAda)