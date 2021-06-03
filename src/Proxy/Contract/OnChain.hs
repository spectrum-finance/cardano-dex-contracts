{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# options_ghc -fno-warn-orphans          #-}
{-# options_ghc -Wno-redundant-constraints #-}
{-# options_ghc -fno-strictness            #-}
{-# options_ghc -fno-specialise            #-}

-- Can be spent only in case:
-- 1. Tx contains dex contract in input
-- 2. Tx contains output with user pubKey
-- 3. Swap satisfy correct conditions

module Proxy.Contract.OnChain where

import           Control.Monad          (void)
import           Playground.Contract    (FromJSON, Generic, ToJSON, ToSchema)
import           GHC.Generics           (Generic)
import           Ledger
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
import qualified Prelude             as Haskell
import           PlutusTx.Prelude
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
      Value )
import qualified Prelude
import           Schema                 (ToArgument, ToSchema)
import           Wallet.Emulator        (Wallet (..))
import           Utils

--todo: rate :: Integer -> rate :: Double ?
--todo: remove ergoToken and adaToken from proxy datum ?
data ProxyDatum = ProxyDatum {
    slippageTolerance :: Integer,
    rate :: Integer,
    userAddressBS :: ByteString,
    ergoToken :: Coin ErgoToken,
    adaToken :: Coin Ada
} deriving (Haskell.Show, Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.makeIsDataIndexed ''ProxyDatum [('ProxyDatum, 0)]
PlutusTx.makeLift ''ProxyDatum

data ProxyAction = Swap | Return
    deriving Haskell.Show

PlutusTx.makeIsDataIndexed ''ProxyAction [ ('Swap ,   0)
                                         , ('Return,    1)
                                         ]
PlutusTx.makeLift ''ProxyAction

{-# INLINABLE findOwnInput' #-}
findOwnInput' :: ScriptContext -> TxInInfo
findOwnInput' ctx = fromMaybe (error ()) (findOwnInput ctx)

{-# INLINABLE checkCorrectSwap #-}
checkCorrectSwap :: ProxyDatum -> ScriptContext -> Bool
checkCorrectSwap ProxyDatum{..} sCtx =
    traceIfFalse "Swap should satisfy conditions" checkConditions
  where
    ownInput :: TxInInfo
    ownInput = findOwnInput' sCtx

    outputWithUserKey :: TxOut
    outputWithUserKey = case [ output
                                     | output <- getContinuingOutputs sCtx
                                     , txOutAddress output == (pubKeyHashAddress $ PubKeyHash userAddressBS)
                                     ] of
      [output]   -> output
      otherwise  -> traceError "expected output with user pubkey"

    isErgoSwap :: Bool
    isErgoSwap =
        let
          outputWithValueToSwap = txInInfoResolved ownInput
          ergoSwapCheck = isUnity (txOutValue outputWithValueToSwap) ergoToken
        in ergoSwapCheck

    checkConditions :: Bool
    checkConditions =
        let
          outputWithValueToSwap = txInInfoResolved ownInput
          inputValue =
              if (isErgoSwap) then outputAmountOf outputWithValueToSwap ergoToken else outputAmountOf outputWithValueToSwap adaToken
          outputValue =
              if (isErgoSwap) then outputAmountOf outputWithUserKey adaToken else outputAmountOf outputWithUserKey ergoToken
          realRate = outputValue `div` inputValue
          -- todo: use double, instead of integer for rate
        in realRate <= rate * slippageTolerance


{-# INLINABLE checkCorrectReturn #-}
checkCorrectReturn :: ProxyDatum -> ScriptContext -> Bool
checkCorrectReturn ProxyDatum{..} sCtx = True

{-# INLINABLE mkProxyValidator #-}
mkProxyValidator :: ProxyDatum -> ProxyAction -> ScriptContext -> Bool
mkProxyValidator proxyDatum Swap sCtx   = checkCorrectSwap proxyDatum sCtx
mkProxyValidator proxyDatum Return sCtx = checkCorrectReturn proxyDatum sCtx

data ProxySwapping
instance Scripts.ScriptType ProxySwapping where
    type instance RedeemerType ProxySwapping = ProxyAction
    type instance DatumType    ProxySwapping = ProxyDatum

proxyInstance :: Scripts.ScriptInstance ProxySwapping
proxyInstance = Scripts.validator @ProxySwapping
    $$(PlutusTx.compile [|| mkProxyValidator ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @ProxyDatum @ProxyAction