{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# options_ghc -fno-warn-orphans          #-}
{-# options_ghc -Wno-redundant-constraints #-}
{-# options_ghc -fno-strictness            #-}
{-# options_ghc -fno-specialise            #-}

module Dex.Contract.Models where

import           Ledger
import           Playground.Contract (FromJSON, Generic, ToJSON, ToSchema)
import qualified PlutusTx
import           Prelude
import qualified PlutusTx.Builtins   as Builtins
import qualified Ledger.Typed.Scripts   as Scripts
import qualified Data.ByteString.Char8  as C

data ErgoDexPool = ErgoDexPool {
    feeNum :: Integer,
    -- determine the hash of first coin
    xCoin :: AssetClass,
    -- determine the hash of second coin
    yCoin :: AssetClass,
    -- determine the hash of lp coin
    lpCoin :: AssetClass
} deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.makeIsDataIndexed ''ErgoDexPool [('ErgoDexPool, 0)]
PlutusTx.makeLift ''ErgoDexPool

data ContractAction = Create | SwapLP | AddTokens | SwapToken
    deriving Show
PlutusTx.makeIsDataIndexed ''ContractAction [ ('Create ,   0)
                                            , ('SwapLP,    1)
                                            , ('AddTokens, 2)
                                            , ('SwapToken,  3)
                                            ]
PlutusTx.makeLift ''ContractAction