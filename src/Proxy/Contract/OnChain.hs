{-# LANGUAGE DataKinds                  #-}
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

-- Can be spent only in case:
-- 1. Tx contains dex contract in input
-- 2. Tx contains output with user pubKey
-- 3. Swap satisfy correct conditions

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
import PlutusTx.Prelude ( Bool(True), Integer, ByteString )
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
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
      Value)
import qualified Prelude
import           Schema                 (ToArgument, ToSchema)
import           Wallet.Emulator        (Wallet (..))
import           Utils
import qualified PlutusTx.Builtins   as Builtins

--todo: rate :: Integer -> rate :: Double ?
--todo: remove ergoToken and adaToken from proxy datum ?
data ProxyDatum = ProxyDatum {
    slippageTolerance :: Integer,
    rate :: Integer,
    userPubKey :: Builtins.ByteString,
    toSwapSymbol :: Builtins.ByteString,
    toSwapTokenName :: Builtins.ByteString,
    -- determine the hash of second coin
    toGetCurSymbol :: Builtins.ByteString,
    toGetTokenName :: Builtins.ByteString
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
    traceIfFalse "Swap should satisfy conditions" True
  where

    coinA :: Coin CoinA
    coinA =
      let
        tokenNameA = tokenName toSwapSymbol
        currencySymbolA = currencySymbol toSwapTokenName
        assetClassA = assetClass currencySymbolA tokenNameA
      in Coin (assetClassA)

    coinB :: Coin CoinB
    coinB =
      let
        tokenNameB = tokenName toGetTokenName
        currencySymbolB = currencySymbol toGetCurSymbol
        assetClassB = assetClass currencySymbolB tokenNameB
      in Coin (assetClassB)

    ownInput :: TxInInfo
    ownInput = findOwnInput' sCtx

    outputWithUserKey :: TxOut
    outputWithUserKey = case [ output
                                     | output <- getContinuingOutputs sCtx
                                     , txOutAddress output == (pubKeyHashAddress $ PubKeyHash userPubKey)
                                     ] of
      [output]   -> output
      otherwise  -> traceError "expected output with user pubkey"

    isASwap :: Bool
    isASwap =
        let
          outputWithValueToSwap = txInInfoResolved ownInput
          ergoSwapCheck = isUnity (txOutValue outputWithValueToSwap) coinA
        in ergoSwapCheck

    checkConditions :: Bool
    checkConditions =
        let
          outputWithValueToSwap = txInInfoResolved ownInput
          inputValue =
              if (isASwap) then outputAmountOf outputWithValueToSwap coinA else outputAmountOf outputWithValueToSwap coinB
          outputValue =
              if (isASwap) then outputAmountOf outputWithUserKey coinB else outputAmountOf outputWithUserKey coinA
          realRate = outputValue `div` inputValue
          -- todo: use double, instead of integer for rate
        in realRate <= rate * slippageTolerance


{-# INLINABLE checkCorrectReturn #-}
checkCorrectReturn :: ProxyDatum -> ScriptContext -> Bool
checkCorrectReturn ProxyDatum{..} sCtx =
  checkTxConstraint (sCtx) (Constraints.MustBeSignedBy (PubKeyHash userPubKey)) &&
  traceIfFalse "Recepient should be issuer" isReturnCorrect
  where

    ownInput :: TxInInfo
    ownInput = findOwnInput' sCtx

    isReturnCorrect :: Bool
    isReturnCorrect =
      let
        value2swap = txOutValue $ txInInfoResolved ownInput
        pubKH = PubKeyHash (userPubKey)
      in checkTxConstraint sCtx (Constraints.MustPayToPubKey pubKH value2swap)

    getTrue :: Bool
    getTrue = True

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