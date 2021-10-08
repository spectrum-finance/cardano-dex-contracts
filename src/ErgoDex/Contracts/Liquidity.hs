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
{-# LANGUAGE NamedFieldPuns             #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module ErgoDex.Contracts.Liquidity where

import qualified Prelude                          as Haskell

import           Ledger
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import qualified Ledger.Ada                       as Ada
import           Ledger.Value                     (AssetClass (..), symbols, assetClassValue)
import           Ledger.Contexts                  (txSignedBy, pubKeyOutput)
import           ErgoDex.Contracts.Types
import           ErgoDex.Contracts.Pool           (PoolState(..), PoolParams(..), mkPoolState, getPoolInput, findPoolDatum)
import qualified PlutusTx
import           PlutusTx.Prelude
import           PlutusTx.IsData.Class
import           Utils

{-# INLINABLE validateLiquidityMinting #-}
validateLiquidityMinting :: Coin Nft -> () -> ScriptContext -> Bool
validateLiquidityMinting poolNft _ ctx =
  traceIfFalse "Minting must be witnessed by pool" (isUnit (txOutValue $ getPoolInput ctx) poolNft)
