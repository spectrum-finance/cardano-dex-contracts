{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE DuplicateRecordFields      #-}

module Proxy.Contract.OffChain where

import           Control.Monad                    hiding (fmap)
import           Playground.Contract
import           Plutus.Contract
import           Ledger                           hiding (singleton)
import qualified PlutusTx
import qualified Data.Map                         as Map
import           PlutusTx.AssocMap                as AssocMap
import qualified PlutusTx
import           Ledger.Constraints               as Constraints
import           PlutusTx.Prelude                 hiding (Semigroup (..), dropWhile, flip, unless)
import           Plutus.Contract
import           Prelude                          as Haskell (Int, Semigroup (..), String, div, dropWhile, flip, show,
                                                              (^), undefined)
import           Data.Proxy                       (Proxy (..))
import           Data.Void                        (Void, absurd)
import qualified PlutusTx.Builtins                as Builtins
import           Data.Text                        (Text, pack)
import           Data.Monoid                      (Last (..))
import           Plutus.V1.Ledger.Value
import           Proxy.Contract.OnChain
import           Proxy.Contract.Models
import           Text.Printf                      (printf)

type ProxyUserSchema =
        Endpoint "swap" SwapParams
        .\/ Endpoint "redeem"   RedeemParams
        .\/ Endpoint "deposit"  DepositParams
        .\/ Endpoint "orders"   InfoParam

data SwapParams = SwapParams {
    toGetCoin :: AssetClass,
    toSwapCoin :: AssetClass,
    swaplpProxyToken :: AssetClass,
    dexFeeDatum :: Integer,
    targetPoolId :: Builtins.ByteString,
    minOutputSwapValue :: Integer,
    toGetCoinAmount :: Integer
} deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data RedeemParams = RedeemParams {
    coinAToRedeem :: AssetClass,
    coinBToRedeem :: AssetClass,
    redeemLpProxyToken :: AssetClass,
    dexFeeDatum :: Integer,
    targetPoolId :: Builtins.ByteString,
    lpProxyTokenAmount :: Integer
} deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data DepositParams = DepositParams {
    coinAToDeposit :: AssetClass,
    coinBToDeposit :: AssetClass,
    depositLpProxyToken :: AssetClass,
    dexFeeDatum :: Integer,
    targetPoolId :: Builtins.ByteString,
    coinAAmount :: Integer,
    coinBAmount :: Integer
} deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data InfoParam = InfoParam {
    infoId :: Int
} deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data Order = Order {
    action :: ProxyAction,
    coinA :: AssetClass,
    coinB :: AssetClass,
    lpCoin :: AssetClass,
    targetPoolId :: Builtins.ByteString
} deriving (Show, Generic, FromJSON, ToJSON)

data UserContractState =
      SwapCreated | RedeemCreated | DepositCreated | Orders [Order]
    deriving (Show, Generic, FromJSON, ToJSON)

proxyAddress :: Ledger.Address
proxyAddress = scriptAddress proxyValidator

proxyHash :: ValidatorHash
proxyHash = validatorHash proxyValidator

swap :: SwapParams -> Contract w s Text ()
swap SwapParams{..} = do
    when (toGetCoin == toSwapCoin) $ throwError "coins must be different"
    when (toGetCoinAmount <= 0) $ throwError "amounts must be positive"
    pkh <- pubKeyHash <$> ownPubKey
    pk <- ownPubKey
    let (PubKeyHash hash) = Ledger.pubKeyHash pk
        emptyValue = Value AssocMap.empty
        inst = proxyInstance
        valueToSpend = assetClassValue toGetCoin toGetCoinAmount
        dat = (ProxyDatum {
            action = Swap,
            minOutputValue = minOutputSwapValue,
            dexFeeDatum = dexFeeDatum,
            userPubKeyHash = hash,
            xProxyToken = toGetCoin,
            yProxyToken = toSwapCoin,
            targetPoolId = targetPoolId,
            lpProxyToken = swaplpProxyToken
        })
        --lookups = Constraints.ownPubKeyHash pkh
        tx = Constraints.mustPayToTheScript dat valueToSpend <>
             Constraints.mustBeSignedBy pkh
    logInfo @String $ printf "Got swap"
    logInfo $ show tx
    ledgerTx <- submitTxConstraints proxyInstance tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "send swap"

redeem :: RedeemParams -> Contract w s Text ()
redeem RedeemParams{..} = do
    when (coinAToRedeem == coinBToRedeem) $ throwError "coins must be different"
    when (lpProxyTokenAmount <= 0) $ throwError "amounts must be positive"
    pkh <- pubKeyHash <$> ownPubKey
    pk <- ownPubKey
    let (PubKeyHash hash) = Ledger.pubKeyHash pk
        emptyValue = Value AssocMap.empty
        inst = proxyInstance
        valueToSpend = assetClassValue redeemLpProxyToken lpProxyTokenAmount
        dat = (ProxyDatum {
            action = Swap,
            minOutputValue = 0,
            dexFeeDatum = dexFeeDatum,
            userPubKeyHash = hash,
            xProxyToken = coinAToRedeem,
            yProxyToken = coinBToRedeem,
            targetPoolId = targetPoolId,
            lpProxyToken = redeemLpProxyToken
        })
        --lookups = Constraints.ownPubKeyHash pkh
        tx = Constraints.mustPayToTheScript dat emptyValue <>
             Constraints.mustBeSignedBy pkh
    logInfo @String $ printf "Got swap"
    logInfo $ show tx
    ledgerTx <- submitTxConstraints proxyInstance tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "send swap"
    logInfo @String $ printf "Handle redeem"

deposit :: DepositParams -> Contract w s Text ()
deposit DepositParams{..} = do
    when (coinAToDeposit == coinBToDeposit) $ throwError "coins must be different"
    when (coinAAmount <= 0 || coinBAmount <= 0) $ throwError "amounts must be positive"
    pkh <- pubKeyHash <$> ownPubKey
    pk <- ownPubKey
    let (PubKeyHash hash) = Ledger.pubKeyHash pk
        emptyValue = Value AssocMap.empty
        valueToSpend = (assetClassValue coinAToDeposit coinAAmount) <> (assetClassValue coinBToDeposit coinBAmount)
        inst = proxyInstance
        dat = (ProxyDatum {
            action = Deposit,
            minOutputValue = 0,
            dexFeeDatum = dexFeeDatum,
            userPubKeyHash = hash,
            xProxyToken = coinAToDeposit,
            yProxyToken = coinBToDeposit,
            targetPoolId = targetPoolId,
            lpProxyToken = depositLpProxyToken
        })
        --lookups = Constraints.ownPubKeyHash pkh
        tx = Constraints.mustPayToTheScript dat valueToSpend
    logInfo @String $ printf "Got deposit"
    logInfo $ show tx
    ledgerTx <- submitTxConstraints proxyInstance tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "Send deposit"

orders :: InfoParam -> Contract w s Text [Order]
orders _ = do
    logInfo @String $ printf "Handle order"
    utxos <- utxoAt proxyAddress
    go $ snd <$> Map.toList utxos
  where
    go :: [TxOutTx] -> Contract w s Text [Order]
    go []       = return []
    go (o : os) = do
        d <- getProxyDatum o
        case d of
            ProxyDatum action _ _ _ coinA coinB poolId coinLp -> do
                let s = Order action coinA coinB coinLp poolId
                logInfo @String $ "found order: " ++ show s
                ss <- go os
                return $ s : ss

getProxyDatum :: TxOutTx -> Contract w s Text ProxyDatum
getProxyDatum o = case txOutDatumHash $ txOutTxOut o of
        Nothing -> throwError "datumHash not found"
        Just h -> case Map.lookup h $ txData $ txOutTxTx o of
            Nothing -> throwError "datum not found"
            Just (Datum e) -> case PlutusTx.fromData e of
                Nothing -> throwError "datum has wrong type"
                Just d  -> return d

userEndpoints :: Contract (Last (Either Text UserContractState)) ProxyUserSchema Void ()
userEndpoints =
    ((f (Proxy @"swap")     (const SwapCreated)         swap                               `select`
      f (Proxy @"redeem")   (const RedeemCreated)       redeem                             `select`
      f (Proxy @"deposit")  (const DepositCreated)      deposit                            `select`
      f (Proxy @"orders")   Orders                      orders                             ) >> userEndpoints)
  where
    f :: forall l a p.
         (HasEndpoint l p ProxyUserSchema, FromJSON p)
      => Proxy l
      -> (a -> UserContractState)
      -> (p -> Contract (Last (Either Text UserContractState)) ProxyUserSchema Text a)
      -> Contract (Last (Either Text UserContractState)) ProxyUserSchema Void ()
    f _ g c = do
        e <- runError $ do
            p <- endpoint @l
            c p
        tell $ Last $ Just $ case e of
            Left err -> Left err
            Right a  -> Right $ g a