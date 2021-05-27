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
{-# LANGUAGE TypeOperators              #-}
{-# options_ghc -fno-warn-orphans          #-}
{-# options_ghc -Wno-redundant-constraints #-}
{-# options_ghc -fno-strictness            #-}
{-# options_ghc -fno-specialise            #-}

module Types where

import           Ledger
import           Ledger.Value        (AssetClass (..), assetClass, assetClassValue, assetClassValueOf)
import           Playground.Contract (FromJSON, Generic, ToJSON, ToSchema)
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude             as Haskell
import           Text.Printf         (PrintfArg)

data ErgoToken = ErgoToken

data LPToken = LPToken

deriving anyclass instance ToSchema AssetClass

PlutusTx.makeIsDataIndexed ''ErgoToken [('ErgoToken, 0)]
PlutusTx.makeLift ''ErgoToken

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

data Pool = Pool {
    adaValue :: Coin Ada,
    ergoValue :: Coin ErgoToken
}