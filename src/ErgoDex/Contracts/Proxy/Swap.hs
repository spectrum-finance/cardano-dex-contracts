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
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module ErgoDex.Contracts.Proxy.Swap where

import qualified Prelude as Haskell

import           Ledger
import           Data.Aeson                    (FromJSON, ToJSON)
import           Ledger.Value                  (assetClassValueOf)
import qualified GHC.Generics                  as GHC
import qualified Ledger.Ada                    as Ada
import           ErgoDex.Contracts.Proxy.Order
import           ErgoDex.Contracts.Pool        (getPoolInput)
import qualified PlutusTx
import           PlutusTx.Prelude

data SwapConfig = SwapConfig
   { base             :: AssetClass
   , quote            :: AssetClass
   , poolNft          :: AssetClass
   , feeNum           :: Integer
   , exFeePerTokenNum :: Integer
   , exFeePerTokenDen :: Integer
   , rewardPkh        :: PubKeyHash
   , stakePkh         :: Maybe PubKeyHash
   , baseAmount       :: Integer
   , minQuoteAmount   :: Integer
   } deriving stock (Haskell.Show, GHC.Generic)
     deriving (FromJSON, ToJSON)
PlutusTx.makeIsDataIndexed ''SwapConfig [('SwapConfig, 0)]
PlutusTx.makeLift ''SwapConfig

{-# INLINABLE mkSwapValidator #-}
mkSwapValidator :: SwapConfig -> BuiltinData -> ScriptContext -> Bool
mkSwapValidator SwapConfig{..} _ ctx =
    txSignedBy txInfo rewardPkh || (
      traceIfFalse "Invalid pool" validPool &&
      traceIfFalse "Invalid number of inputs" validNumInputs &&
      traceIfFalse "Invalid reward proposition" validRewardProp &&
      traceIfFalse "Unfair execution fee" fairExFee && -- true
      traceIfFalse "Min output not met" (quoteAmount >= minQuoteAmount) && -- true
      traceIfFalse "Unfair execution price" fairPrice -- true
    )
  where
    txInfo = scriptContextTxInfo ctx
    self   = findOrderInput ctx
    pool   = getPoolInput ctx poolNft
    reward = findRewardInput ctx rewardPkh

    poolValue = txOutValue pool

    validPool = assetClassValueOf poolValue poolNft == 1 -- true

    validNumInputs = length (txInfoInputs txInfo) == 2 -- true

    validRewardProp = maybe False (== rewardPkh) (pubKeyOutput reward) -- true

    selfValue   = txOutValue self
    rewardValue = txOutValue reward

    quoteAmount =
        if isAda quote
          then divide (quoteDelta * exFeePerTokenDen) (exFeePerTokenDen - exFeePerTokenNum)
          else quoteDelta -- 4739223624  -- 4607548043

      where
        quoteOut   = assetClassValueOf rewardValue quote -- 4739223624
        quoteIn    = assetClassValueOf selfValue quote -- 0
        quoteDelta = quoteOut - quoteIn -- 4739223624

    fairExFee =
        outAda - quoteAda >= inAda - baseAda - exFee -- (1687642 - 0) >= 1687642 -- true 
      where
        (baseAda, quoteAda)
          | isAda base  = (baseAmount, 0) -- 50000000, 0
          | isAda quote = (0, quoteAmount)
          | otherwise   = (0, 0)
        outAda = Ada.getLovelace $ Ada.fromValue rewardValue -- 1687642
        inAda  = Ada.getLovelace $ Ada.fromValue selfValue -- 53744798
        exFee  = divide (quoteAmount * exFeePerTokenNum) exFeePerTokenDen -- (4739223624 * 434070351808592) / 1000000000000000000

    fairPrice =
        reservesQuote * baseAmount * feeNum <= relaxedOut * (reservesBase * feeDen + baseAmount * feeNum)
      -- 100000000000 * 50000000 * 434070351808592 <= 4739223625 * (1000000000 * 1000000000000000000 + 50000000 * 434070351808592)
      -- 2170351759042960000000000000000000 <= 
      -- 21703517590429600000000000000000000000000000000000
      where -- 1000000000000000000000000000 -- 21703517590429600000000
        relaxedOut    = quoteAmount + 1  -- 4739223625
        reservesBase  = assetClassValueOf poolValue base -- 1000000000
        reservesQuote = assetClassValueOf poolValue quote -- 100000000000
        feeDen        = 1000
