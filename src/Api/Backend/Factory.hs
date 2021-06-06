{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}


module Api.Backend.Factory where

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
      AsContractError,
      logInfo,
      submitTxConstraintsWith,
      HasBlockchainActions
    )
import PlutusTx.Prelude ( Bool(True), Integer, ByteString )
import           PlutusTx.IsData
import           Data.Text                        (Text, pack)
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import           Plutus.Contract.Schema ()
import           Plutus.Trace.Emulator  (EmulatorTrace)
import qualified Plutus.Trace.Emulator  as Trace
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude             as Haskell
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
import Proxy.Contract.OnChain
import Dex.Contract.OffChain
import           Playground.Contract
import           Ledger.Scripts  (unitRedeemer)


-- createSwapTransaction :: TxOut -> Datum -> TxOutRef -> TxOut -> Datum -> Tx
-- createSwapTransaction outWithProxy proxyDatum proxyTxOutRef outWithPool poolDatum = do
--     let tx = Constraints.mustSpendScriptOutput proxyTxOutRef (Redeemer $ PlutusTx.toData Swap)
--     logInfo $ show tx
--     ledgerTx <- submitTxConstraintsWith lookups tx
--     logInfo $ show ledgerTx

createSwapTransaction :: HasBlockchainActions s => TxOutRef -> Contract w s Text Tx
createSwapTransaction proxyTxOutRef =  do
    let
        lookups = Constraints.otherScript (Scripts.validatorScript dexInstance)
        tx = Constraints.mustSpendScriptOutput proxyTxOutRef unitRedeemer
    ledgerTx <- submitTxConstraintsWith lookups tx
    return ledgerTx

    -- ledgerTx <- submitTxConstraintsWith lookups tx
    -- logInfo $ "created liquidity pool: " ++ show ledgerTx