{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.PContracts.PSwap
  ( SwapConfig(..)
  , swapValidator
  ) where

import qualified GHC.Generics as GHC
import Plutarch
import Plutarch.Prelude
import Plutarch.DataRepr
import Plutarch.Api.V1.Contexts
import Plutarch.Api.V1
import PExtra.API
import PExtra.Monadic (tlet, tletField)
import Generics.SOP (Generic, I (I))
import PExtra.Ada ( pGetLovelace, pIsAda )

import ErgoDex.PContracts.PApi
import ErgoDex.PContracts.POrder

newtype SwapConfig (s :: S) = SwapConfig
  (
    Term s (
      PDataRecord
        '[ "base"             ':= PAssetClass
         , "quote"            ':= PAssetClass
         , "poolNft"          ':= PAssetClass
         , "feeNum"           ':= PInteger
         , "exFeePerTokenNum" ':= PInteger
         , "exFeePerTokenDen" ':= PInteger
         , "rewardPkh"        ':= PPubKeyHash
         , "baseAmount"       ':= PInteger
         , "minQuoteAmount"   ':= PInteger
        ]
    )
  )  
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PMatch, PIsData, PDataFields, PlutusType)
    via (PIsDataReprInstances SwapConfig)

swapValidator :: Term s (SwapConfig :--> OrderRedeemer :--> PScriptContext :--> PBool)
swapValidator = plam $ \configT redeemerT cxtT -> unTermCont $ do
  txInfo    <- tletField @"txInfo" cxtT
  rewardPkh <- tletField @"rewardPkh" configT

  isTxSignedByRewardKey <- tlet $ hasValidSignatories # txInfo # rewardPkh

  pure $ isTxSignedByRewardKey #|| (isValidSwap # txInfo # rewardPkh # redeemerT # configT)

isValidSwap :: Term s (PTxInfo :--> PPubKeyHash :--> OrderRedeemer :--> SwapConfig :--> PBool)
isValidSwap = plam $ \txInfo rewardPkh redeemerT configT -> unTermCont $ do
  indexes <- tcont $ pletFields @'["orderInIx", "poolInIx", "rewardOutIx"] redeemerT
  cfg     <- tcont $ pletFields @'["base", "quote", "poolNft", "exFeePerTokenNum", "exFeePerTokenDen", "baseAmount"] configT

  orderInIx  <- tletUnwrap $ hrecField @"orderInIx" indexes
  poolInIx   <- tletUnwrap $ hrecField @"poolInIx" indexes
  rewardOutIx <- tletUnwrap $ hrecField @"rewardOutIx" indexes

  rewardValue <- tlet $ getRewardValue # txInfo # rewardOutIx # rewardPkh

  inputs     <- tletField @"inputs" txInfo
  poolValue  <- tlet $ getInputValue # inputs # poolInIx
  orderValue <- tlet $ getInputValue # inputs # orderInIx

  base             <- tletUnwrap $ hrecField @"base" cfg
  quote            <- tletUnwrap $ hrecField @"quote" cfg 
  exFeePerTokenNum <- tletUnwrap $ hrecField @"exFeePerTokenNum" cfg
  exFeePerTokenDen <- tletUnwrap $ hrecField @"exFeePerTokenDen" cfg
  baseAmount       <- tletUnwrap $ hrecField @"baseAmount" cfg
  quoteAmount      <- tlet $ calcQuoteAmount # rewardValue # orderValue # quote # exFeePerTokenDen # exFeePerTokenNum

  let 
    fairExFee    = calcFairExFee # rewardValue # orderValue # base # baseAmount # quote # quoteAmount # exFeePerTokenNum # exFeePerTokenDen
    fairPrice    = calcFairPrice # quoteAmount # poolValue # base # quote # baseAmount # configT
    correctQuote = calcCorrectQuote # configT # quoteAmount

  poolNft  <- tletUnwrap $ hrecField @"poolNft" cfg
  let
    validNft    = checkPoolNft # poolValue # poolNft
    validInputs = checkInputsQty # inputs

  pure $ validNft #&& validInputs #&& fairExFee #&& fairPrice #&& correctQuote

calcCorrectQuote :: Term s (SwapConfig :--> PInteger :--> PBool)
calcCorrectQuote = plam $ \cfg quoteAmount ->
  let minQuoteAmount = pfield @"minQuoteAmount" # cfg
  in quoteAmount #< minQuoteAmount

calcFairExFee 
  :: Term s (
         PValue 
    :--> PValue 
    :--> PAssetClass 
    :--> PInteger 
    :--> PAssetClass 
    :--> PInteger 
    :--> PInteger 
    :--> PInteger 
    :--> PBool
    )
calcFairExFee =
  plam $ \rewardValue orderValue base baseAmount quote quoteAmount exFeePerTokenNum exFeePerTokenDen ->
    unTermCont $ do
      zeroAsData' <- tlet zeroAsData
      bqAda       <- tlet $ pif (pIsAda # base) 
        (ptuple # pdata baseAmount # zeroAsData')
        (pif (pIsAda # quote) (ptuple # zeroAsData' # pdata quoteAmount) (ptuple # zeroAsData' # zeroAsData'))
      let
        baseAda  = pfromData $ pfield @"_0" # bqAda
        quoteAda = pfromData $ pfield @"_1" # bqAda
        outAda   = pGetLovelace # rewardValue
        inAda    = pGetLovelace # orderValue
        exFee    = pdiv # (quoteAmount * exFeePerTokenNum) # exFeePerTokenDen
      pure $ (inAda - baseAda - exFee) #< (outAda - quoteAda)
  
calcFairPrice 
  :: Term s (
         PInteger 
    :--> PValue 
    :--> PAssetClass 
    :--> PAssetClass 
    :--> PInteger
    :--> SwapConfig 
    :--> PBool
    )
calcFairPrice =
  plam $ \quoteAmount poolValue base quote baseAmount configT ->
    unTermCont $ do
      feeNum <- tlet $ pfield @"feeNum" # configT
      let
        relaxedOut    = quoteAmount + 1
        reservesBase  = assetClassValueOf # poolValue # base
        reservesQuote = assetClassValueOf # poolValue # quote
      pure $ reservesQuote * baseAmount * feeNum #<= relaxedOut * (reservesBase * feeDen + baseAmount * feeNum)

calcQuoteAmount :: Term s (PValue :--> PValue :--> PAssetClass :--> PInteger :--> PInteger :--> PInteger)
calcQuoteAmount =
  plam $ \rewardValue orderValue quote exFeePerTokenDen exFeePerTokenNum ->
    unTermCont $ do
      let
        quoteOut = assetClassValueOf # rewardValue # quote
        quoteIn  = assetClassValueOf # orderValue # quote
      quoteDelta <- tlet $ quoteOut - quoteIn
      pure $ pif (pIsAda # quote)
        (pdiv # (quoteDelta * exFeePerTokenDen) # (exFeePerTokenDen - exFeePerTokenNum))
        quoteDelta  
