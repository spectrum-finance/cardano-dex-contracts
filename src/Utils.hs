{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
{-# options_ghc -fno-warn-orphans          #-}
{-# options_ghc -Wno-redundant-constraints #-}
{-# options_ghc -fno-strictness            #-}
{-# options_ghc -fno-specialise            #-}

module Utils where


import           Ledger
import           Ledger.Value        (AssetClass (..), assetClass, assetClassValue, assetClassValueOf)
import           Playground.Contract (FromJSON, Generic, ToJSON, ToSchema)
import qualified PlutusTx
import PlutusTx.Prelude
    ( Bool,
      Integer,
      (.),
      ($),
      Eq(..),
      AdditiveGroup,
      AdditiveMonoid,
      AdditiveSemigroup,
      MultiplicativeSemigroup,
      fromMaybe,
      error,
      Ord )
import qualified Prelude             as Haskell
import           Text.Printf         (PrintfArg)
import qualified Data.ByteString.Char8  as C
import qualified PlutusTx.Builtins   as Builtins
import qualified Ledger.Typed.Scripts   as Scripts
import           Plutus.V1.Ledger.Address
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.TxId
import           Plutus.V1.Ledger.Scripts
import qualified PlutusTx
import           PlutusTx.Prelude
import           Data.ByteString.Hash

newtype PoolId = PoolId Builtins.ByteString
    deriving (Haskell.Show, Generic, FromJSON, ToJSON, Haskell.Eq)

data CoinA = CoinA

PlutusTx.makeIsDataIndexed ''CoinA [('CoinA, 0)]
PlutusTx.makeLift ''CoinA

data CoinB = CoinB

PlutusTx.makeIsDataIndexed ''CoinB [('CoinB, 0)]
PlutusTx.makeLift ''CoinB

data LPToken = LPToken

PlutusTx.makeIsDataIndexed ''LPToken [('LPToken, 0)]
PlutusTx.makeLift ''LPToken

newtype Coin a = Coin { unCoin :: AssetClass }
  deriving stock   (Haskell.Show, Generic)
  deriving newtype (ToJSON, FromJSON, ToSchema, Eq, Haskell.Eq, Haskell.Ord)
PlutusTx.makeIsDataIndexed ''Coin [('Coin, 0)]
PlutusTx.makeLift ''Coin

newtype Amount a = Amount { unAmount :: Integer }
  deriving stock   (Haskell.Show, Generic)
  deriving newtype (ToJSON, FromJSON, ToSchema, Eq, Ord, PrintfArg)
  deriving newtype (Haskell.Eq, Haskell.Ord, Haskell.Num)
  deriving newtype (AdditiveGroup, AdditiveMonoid, AdditiveSemigroup, MultiplicativeSemigroup)
PlutusTx.makeIsDataIndexed ''Amount [('Amount, 0)]
PlutusTx.makeLift ''Amount

{-# INLINABLE amountOf #-}
amountOf :: Value -> Coin a -> Amount a
amountOf v = Amount . assetClassValueOf v . unCoin

{-# INLINABLE outputAmountOf #-}
outputAmountOf :: TxOut -> Coin a -> Integer
outputAmountOf out coin = unAmount $ amountOf (txOutValue out) coin

{-# INLINABLE isUnity #-}
isUnity :: Value -> Coin a -> Bool
isUnity v c = amountOf v c == 1

{-# INLINABLE mkCoin #-}
mkCoin :: CurrencySymbol -> TokenName -> Coin a
mkCoin c = Coin . assetClass c

{-# INLINABLE findOwnInput' #-}
findOwnInput' :: ScriptContext -> TxInInfo
findOwnInput' ctx = fromMaybe (error ()) (findOwnInput ctx)

{-# INLINABLE valueWithin #-}
valueWithin :: TxInInfo -> Value
valueWithin = txOutValue . txInInfoResolved

{-# INLINABLE lpSupply #-}
lpSupply :: Integer
lpSupply = 0x7fffffffffffffff

{-# INLINABLE calculateValueInOutputs #-}
calculateValueInOutputs :: [TxInInfo] -> Coin a -> Integer
calculateValueInOutputs outputs coinValue =
    foldl getAmountAndSum (0 :: Integer) outputs
  where
    getAmountAndSum :: Integer -> TxInInfo -> Integer
    getAmountAndSum acc out = acc `Builtins.addInteger` unAmount (amountOf (txOutValue $ txInInfoResolved out) coinValue)

{-# INLINABLE inputsLockedByDatumHash #-}
inputsLockedByDatumHash :: DatumHash -> ScriptContext -> [TxInInfo]
inputsLockedByDatumHash hash sCtx = [ proxyInput
                                    | proxyInput <- txInfoInputs (scriptContextTxInfo sCtx)
                                    , txOutDatumHash (txInInfoResolved proxyInput) == Just hash
                                    ]

{-# INLINABLE inputsLockedByUserPubKeyHash #-}
inputsLockedByUserPubKeyHash :: PubKeyHash -> ScriptContext -> [TxOut]
inputsLockedByUserPubKeyHash pubKeyHash sCtx = [ output
                                               | output <- getContinuingOutputs sCtx
                                               , txOutAddress output == (pubKeyHashAddress pubKeyHash)
                                               ]

{-# INLINABLE ownOutput #-}
ownOutput :: ScriptContext -> TxOut
ownOutput sCtx = case [ o
                      | o <- getContinuingOutputs sCtx
                      , txOutDatumHash o == Just (snd $ ownHashes sCtx)
                      ] of
                [o] -> o
                _   -> traceError "expected exactly one output to the same liquidity pool"

{-# INLINABLE check2inputs #-}
check2inputs :: ScriptContext -> Bool
check2inputs sCtx = length (txInfoInputs $ (scriptContextTxInfo sCtx)) == 2

{-# INLINABLE check2outputs #-}
check2outputs :: ScriptContext -> Bool
check2outputs sCtx = length (txInfoOutputs $ (scriptContextTxInfo sCtx)) == 2