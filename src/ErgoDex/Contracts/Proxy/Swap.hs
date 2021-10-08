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

module ErgoDex.Contracts.Proxy.Swap where

import qualified Prelude                          as Haskell

import           Ledger
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import qualified Ledger.Ada                       as Ada
import qualified Ledger.Typed.Scripts             as Scripts
import           Ledger.Value                     (AssetClass (..), symbols, assetClassValue)
import           Ledger.Contexts                  (txSignedBy, pubKeyOutput)
import           ErgoDex.Contracts.Types
import           ErgoDex.Contracts.Pool           (getPoolInput)
import qualified PlutusTx
import           PlutusTx.Prelude
import           PlutusTx.IsData.Class
import           Utils

data SwapDatum = SwapDatum
   { base             :: Coin Base
   , quote            :: Coin Quote
   , poolNft          :: Coin Nft
   , feeNum           :: Integer
   , exFeePerTokenNum :: Integer
   , exFeePerTokenDen :: Integer
   , rewardPkh        :: PubKeyHash
   , minQuoteAmount   :: Amount Quote
   } deriving stock (Haskell.Show)
PlutusTx.makeIsDataIndexed ''SwapDatum [('SwapDatum, 0)]
PlutusTx.makeLift ''SwapDatum

{-# INLINABLE mkSwapValidator #-}
mkSwapValidator :: SwapDatum -> BuiltinData -> ScriptContext -> Bool
mkSwapValidator SwapDatum{..} _ ctx =
    txSignedBy txInfo rewardPkh || (
      traceIfFalse "Invalid pool" validPool &&
      traceIfFalse "Invalid number of inputs" validNumInputs &&
      traceIfFalse "Invalid reward proposition" validRewardProp &&
      traceIfFalse "Unfair execution fee" fairExFee &&
      traceIfFalse "Min output not met" (quoteAmount >= unAmount minQuoteAmount) &&
      traceIfFalse "Unfair execution price" fairPrice
    )
  where
    txInfo = scriptContextTxInfo ctx
    self   = txInInfoResolved $ findOwnInput' ctx
    pool   = getPoolInput ctx
    reward = (txInfoOutputs txInfo) !! 1 -- reward box is always 2nd output

    poolValue = txOutValue pool

    validPool = isUnit poolValue poolNft

    validNumInputs = (length $ txInfoInputs txInfo) == 2

    validRewardProp = maybe False (== rewardPkh) (pubKeyOutput reward)

    selfValue   = txOutValue self
    rewardValue = txOutValue reward

    baseAmount  = valueOf selfValue base
    quoteAmount = valueOf rewardValue quote

    fairExFee =
        outAda >= inAda - exFee
      where
        outAda = Ada.getLovelace $ Ada.fromValue rewardValue
        inAda  = Ada.getLovelace $ Ada.fromValue selfValue
        exFee  = divide (quoteAmount * exFeePerTokenNum) exFeePerTokenDen

    fairPrice =
        reservesQuote * baseAmount * feeNum <= relaxedOut * (reservesBase * feeDen + baseAmount * feeNum)
      where
        relaxedOut    = quoteAmount + 1
        reservesBase  = valueOf poolValue base
        reservesQuote = valueOf poolValue quote
        feeDen        = 1000
