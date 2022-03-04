{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.PContracts.PSwap where

import qualified GHC.Generics as GHC
import Plutarch
import Plutarch.Prelude
import Plutarch.DataRepr
import Plutarch.Api.V1.Contexts
import Plutarch.Api.V1
import PExtra.API
import PExtra.Monadic (tlet, tletField)
import Generics.SOP (Generic, I (I))
import PExtra.Ada

import ErgoDex.PContracts.PApi

newtype PSwapRedeemer (s :: S) = PSwapRedeemer
  (
    Term s (
      PDataRecord
      '[ "orderIndex"  ':= PInteger
       , "poolIndex"   ':= PInteger
       , "rewardIndex" ':= PInteger
      ]
    )
  )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PMatch, PIsData, PDataFields, PlutusType)
    via (PIsDataReprInstances PSwapRedeemer)

newtype PSwapConfig (s :: S) = PSwapConfig
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
    via (PIsDataReprInstances PSwapConfig)

mkSwapValidator :: Term s (PSwapConfig :--> PSwapRedeemer :--> PScriptContext :--> PBool)
mkSwapValidator = plam $ \configT redeemerT cxtT -> unTermCont $ do
  txInfo    <- tletField @"txInfo" cxtT
  rewardPkh <- tletField @"rewardPkh" configT

  isTxSignedByRewardKey <- tlet $ hasValidSignatories # txInfo # rewardPkh

  pure $ isTxSignedByRewardKey #|| (isValidSwap # txInfo # rewardPkh # redeemerT # configT)

isValidSwap :: Term s (PTxInfo :--> PPubKeyHash :--> PSwapRedeemer :--> PSwapConfig :--> PBool)
isValidSwap = plam $ \txInfo rewardPkh redeemerT configT -> unTermCont $ do
  indexes <- tcont $ pletFields @'["orderIndex", "poolIndex", "rewardIndex"] redeemerT
  cfg     <- tcont $ pletFields @'["base", "quote", "poolNft", "exFeePerTokenNum", "exFeePerTokenDen", "baseAmount"] configT

  orderIndex  <- tletUnwrap $ hrecField @"orderIndex" indexes
  poolIndex   <- tletUnwrap $ hrecField @"poolIndex" indexes
  rewardIndex <- tletUnwrap $ hrecField @"rewardIndex" indexes

  rewardValue <- tlet $ getRewardValue # txInfo # rewardIndex # rewardPkh

  inputs     <- tletField @"inputs" txInfo
  poolValue  <- tlet $ getInputValue # inputs # poolIndex
  orderValue <- tlet $ getInputValue # inputs # orderIndex

  base             <- tletUnwrap $ hrecField @"base" cfg
  quote            <- tletUnwrap $ hrecField @"quote" cfg 
  exFeePerTokenNum <- tletUnwrap $ hrecField @"exFeePerTokenNum" cfg
  exFeePerTokenDen <- tletUnwrap $ hrecField @"exFeePerTokenDen" cfg
  baseAmount       <- tletUnwrap $ hrecField @"baseAmount" cfg
  
  let 
    quoteAmount  = calcQuoteAmount # rewardValue # orderValue # quote # exFeePerTokenDen # exFeePerTokenNum
    fairExFee    = calcFairExFee # rewardValue # orderValue # base # baseAmount # quote # quoteAmount # exFeePerTokenNum # exFeePerTokenDen
    fairPrice    = calcFairPrice # quoteAmount # poolValue # base # quote # baseAmount # configT
    correctQuote = calcCorrectQuote # configT # quoteAmount

  poolNft <- tletUnwrap $ hrecField @"poolNft" cfg
  _       <- tlet $ poolCheckNft # poolValue # poolNft
  _       <- tlet $ validInputsQty # inputs

  pure $ fairExFee #&& fairPrice #&& correctQuote

calcCorrectQuote :: Term s (PSwapConfig :--> PInteger :--> PBool)
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
calcFairExFee = plam $ \rewardValue orderValue base baseAmount quote quoteAmount exFeePerTokenNum exFeePerTokenDen ->
  let
    bqAda  = 
      pif (pIsAda # base) 
        (ptuple # pdata baseAmount # zeroInteger)
        (pif (pIsAda # quote) (ptuple # zeroInteger # pdata quoteAmount) (ptuple # zeroInteger # zeroInteger))
    baseAda  = pfromData $ pfield @"_0" # bqAda
    quoteAda = pfromData $ pfield @"_1" # bqAda
    outAda = pGetLovelace # rewardValue
    inAda  = pGetLovelace # orderValue
    exFee  = pdiv # (quoteAmount * exFeePerTokenNum) # exFeePerTokenDen
  in 
    (inAda - baseAda - exFee) #< (outAda - quoteAda)
  
calcFairPrice 
  :: Term s (
         PInteger 
    :--> PValue 
    :--> PAssetClass 
    :--> PAssetClass 
    :--> PInteger
    :--> PSwapConfig 
    :--> PBool
    )
calcFairPrice = plam $ \quoteAmount poolValue base quote baseAmount configT ->
  let
    feeNum        = pfield @"feeNum" # configT
    relaxedOut    = quoteAmount + 1
    reservesBase  = assetClassValueOf # poolValue # base
    reservesQuote = assetClassValueOf # poolValue # quote
    feeDen        = 1000
  in
    reservesQuote * baseAmount * feeNum #<= relaxedOut * (reservesBase * feeDen + baseAmount * feeNum)

calcQuoteAmount :: Term s (PValue :--> PValue :--> PAssetClass :--> PInteger :--> PInteger :--> PInteger)
calcQuoteAmount = plam $ \rewardValue orderValue quote exFeePerTokenDen exFeePerTokenNum ->
  let
    quoteOut   = assetClassValueOf # rewardValue # quote
    quoteIn    = assetClassValueOf # orderValue # quote
    quoteDelta = quoteOut - quoteIn
  in
    pif (pIsAda # quote) (pdiv # (quoteDelta * exFeePerTokenDen) # (exFeePerTokenDen - exFeePerTokenNum)) quoteDelta  
