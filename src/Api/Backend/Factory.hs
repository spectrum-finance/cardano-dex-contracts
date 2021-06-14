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

import           Control.Monad                    hiding (fmap)
import qualified Data.Map                         as Map
import           Data.Map                         (Map)
import           Data.Text                        (Text, pack)
import           Ledger                           hiding (singleton)
import qualified Data.ByteString.Char8  as C
import           Ledger.Constraints               as Constraints
import qualified Ledger.Typed.Scripts             as Scripts
import           Control.Monad.Freer              (Eff, Member)
import           Playground.Contract
import           PlutusTx.IsData
import           Plutus.Contract                  hiding (when)
import           Plutus.Contract.Wallet           (balanceTx)
import qualified PlutusTx
import           PlutusTx.Prelude                 hiding (Semigroup (..), dropWhile, flip, unless)
import           Prelude                          as Haskell (Int, Semigroup (..), String, div, dropWhile, flip, show,
                                                              (^))
import           Text.Printf                      (printf)
import           Proxy.Contract.OnChain
import           Dex.Contract.OffChain
import           Ledger.Scripts  (unitRedeemer)
import           Ledger.Ada
import           Wallet.Effects
import           Control.Monad.Freer.Error      (Error, throwError)
import           Control.Monad.Freer.Extras.Log (LogMsg, logDebug, logInfo)
import           Wallet.Emulator.LogMessages    (TxBalanceMsg (..))

-- createSwapTransaction :: TxOut -> Datum -> TxOutRef -> TxOut -> Datum -> Tx
-- createSwapTransaction outWithProxy proxyDatum proxyTxOutRef outWithPool poolDatum = do
--     let tx = Constraints.mustSpendScriptOutput proxyTxOutRef (Redeemer $ PlutusTx.toData Swap)
--     logInfo $ show tx
--     ledgerTx <- submitTxConstraintsWith lookups tx
--     logInfo $ show ledgerTx

createSwapTransaction :: TxOutRef -> ProxyDatum -> Datum -> TxOutTx -> Either MkTxError UnbalancedTx
createSwapTransaction proxyTxOutRef proxyDatum datum o =
    let
        value = lovelaceValueOf 10
        lookups  = Constraints.scriptInstanceLookups proxyInstance <>
                   Constraints.otherData datum <>
                   Constraints.otherScript (Scripts.validatorScript proxyInstance) <>
                   Constraints.unspentOutputs (Map.singleton proxyTxOutRef o)

        redeemer = Redeemer $ PlutusTx.toData Swap

        tx =  Constraints.mustSpendScriptOutput proxyTxOutRef redeemer <>
              Constraints.mustPayToTheScript proxyDatum value


        unTx = Constraints.mkTx @ProxySwapping lookups tx
    in unTx

    -- ledgerTx <- submitTxConstraintsWith lookups tx
    --

-- tryBalance :: ( Member WalletEffect effs
--     , Member (Error WalletAPIError) effs
--     , Member ChainIndexEffect effs
--     , Member (LogMsg TxBalanceMsg) effs
--     ) => UnbalancedTx -> PubKey -> Eff effs Tx
-- tryBalance unTx pkNotToUse = balanceTx Map.empty pkNotToUse unTx