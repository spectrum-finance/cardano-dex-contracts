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
      Ord )
import qualified Prelude             as Haskell
import           Text.Printf         (PrintfArg)
import           Dex.Contract.Models
import qualified Data.ByteString.Char8  as C
import qualified PlutusTx.Builtins   as Builtins
import           Plutus.V1.Ledger.Address
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.TxId
import           Plutus.V1.Ledger.Scripts

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

amountOf :: Value -> Coin a -> Amount a
amountOf v = Amount . assetClassValueOf v . unCoin

outputAmountOf :: TxOut -> Coin a -> Integer
outputAmountOf out = assetClassValueOf (txOutValue out) . unCoin

isUnity :: Value -> Coin a -> Bool
isUnity v c = amountOf v c == 1

{-# INLINABLE mkCoin #-}
mkCoin:: CurrencySymbol -> TokenName -> Coin a
mkCoin c = Coin . assetClass c

getCoinAFromPool :: ErgoDexPool -> Coin CoinA
getCoinAFromPool ErgoDexPool{..} = Coin (xCoin)

getCoinBFromPool :: ErgoDexPool -> Coin CoinB
getCoinBFromPool ErgoDexPool{..} = Coin (yCoin)

getCoinLPFromPool :: ErgoDexPool -> Coin LPToken
getCoinLPFromPool ErgoDexPool{..} = Coin (lpCoin)