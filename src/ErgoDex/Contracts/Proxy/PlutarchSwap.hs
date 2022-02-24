{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.Contracts.Proxy.PlutarchSwap where

import qualified GHC.Generics as GHC
import Plutarch
import Plutarch.Prelude
import Plutarch.DataRepr
import Plutarch.Api.V1.Contexts
import Plutarch.Api.V1
import PExtra.API
import PExtra.Monadic (tlet, tletField)
import PExtra.List
import Generics.SOP (Generic, I (I))
import PExtra.Ada

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

tletUnwrap :: (PIsData a) => Term s (PAsData a) -> TermCont @r s (Term s a)
tletUnwrap = tlet . pfromData

pMkSwapValidator :: Term s (PSwapConfig :--> PSwapRedeemer :--> PScriptContext :--> PBool)
pMkSwapValidator = plam $ \configT redeemerT cxtT -> unTermCont $ do
  txInfo  <- tletField @"txInfo" cxtT
  indexes <- tcont $ pletFields @'["orderIndex", "poolIndex", "rewardIndex"] redeemerT
  cfg     <- tcont $ pletFields @'["base", "quote", "poolNft", "feeNum", "exFeePerTokenNum", "exFeePerTokenDen",  "rewardPkh", "baseAmount", "minQuoteAmount"] configT

  inputs      <- tletField @"inputs" txInfo
  outputs     <- tletField @"outputs" txInfo

  orderIndex  <- tletUnwrap $ hrecField @"orderIndex" indexes
  poolIndex   <- tletUnwrap $ hrecField @"poolIndex" indexes
  rewardIndex <- tletUnwrap $ hrecField @"rewardIndex" indexes

  orderInput <- tletUnwrap $ pelemAt # orderIndex # inputs
  poolInput  <- tletUnwrap $ pelemAt # poolIndex # inputs

  orderResolved <- tletField @"resolved" orderInput
  poolResolved <- tletField @"resolved" poolInput

  rewardOut <- tletUnwrap $ pelemAt # rewardIndex # outputs
  
  base             <- tletUnwrap $ hrecField @"base" cfg
  quote            <- tletUnwrap $ hrecField @"quote" cfg
  feeNum           <- tletUnwrap $ hrecField @"feeNum" cfg
  exFeePerTokenNum <- tletUnwrap $ hrecField @"exFeePerTokenNum" cfg
  exFeePerTokenDen <- tletUnwrap $ hrecField @"exFeePerTokenDen" cfg
  baseAmount       <- tletUnwrap $ hrecField @"baseAmount" cfg
  minQuoteAmount   <- tletUnwrap $ hrecField @"minQuoteAmount" cfg
  rewardPkh        <- tletUnwrap $ hrecField @"rewardPkh" cfg
  poolNft          <- tletUnwrap $ hrecField @"poolNft" cfg

  poolValue   <- tletField @"value" poolResolved
  rewardValue <- tletField @"value" rewardOut
  orderValue  <- tletField @"value" orderResolved

  rewardAddress <- tletField @"address" rewardOut

  let
    rewardPKH       = pToPubKeyHash # rewardAddress
    validRewardProp = rewardPKH #== rewardPkh
    validPool = plet (assetClassValueOf # poolValue # poolNft) $ \nftQty -> 
                        pif (nftQty #== 1) (pcon PTrue) perror
    validNumInputs = plet (plength # inputs) $ \outsNum -> pif (outsNum #== 2) (pcon PTrue) perror
    quoteAmount = pQuoteAmount # rewardValue # orderValue # quote # exFeePerTokenDen # exFeePerTokenNum

    fairExFee = pFairExFee # rewardValue # orderValue # base # baseAmount # quote # quoteAmount # exFeePerTokenNum # exFeePerTokenDen
            
    correctQuote = quoteAmount #< minQuoteAmount

    fairPrice = pFairPrice # quoteAmount # poolValue # base # quote # feeNum # baseAmount

    isTxSignedByRewardKey = pTxSignedBy # txInfo # rewardPKH

  pure $ isTxSignedByRewardKey #|| (validPool #&& validNumInputs #&& validRewardProp #&& fairExFee #&& correctQuote #&& fairPrice)

pFairExFee 
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
pFairExFee = plam $ \rewardValue orderValue base baseAmount quote quoteAmount exFeePerTokenNum exFeePerTokenDen ->
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
  
pFairPrice 
  :: Term s (
         PInteger 
    :--> PValue 
    :--> PAssetClass 
    :--> PAssetClass 
    :--> PInteger 
    :--> PInteger 
    :--> PBool
    )
pFairPrice = plam $ \quoteAmount poolValue base quote feeNum baseAmount ->
  let
    relaxedOut    = quoteAmount + 1
    reservesBase  = assetClassValueOf # poolValue # base
    reservesQuote = assetClassValueOf # poolValue # quote
    feeDen        = 1000
  in
    reservesQuote * baseAmount * feeNum #<= relaxedOut * (reservesBase * feeDen + baseAmount * feeNum)

pQuoteAmount :: Term s (PValue :--> PValue :--> PAssetClass :--> PInteger :--> PInteger :--> PInteger)
pQuoteAmount = plam $ \rewardValue orderValue quote exFeePerTokenDen exFeePerTokenNum ->
  let
    quoteOut   = assetClassValueOf # rewardValue # quote
    quoteIn    = assetClassValueOf # orderValue # quote
    quoteDelta = quoteOut - quoteIn
  in
    pif (pIsAda # quote) (pdiv # (quoteDelta * exFeePerTokenDen) # (exFeePerTokenDen - exFeePerTokenNum)) quoteDelta  

zeroInteger :: Term s (PAsData PInteger)
zeroInteger = pdata 0

pToPubKeyHash :: Term s (PAddress :--> PPubKeyHash)
pToPubKeyHash = plam $ \address ->
  let credential = pfromData $ pfield @"credential" # address
  in pmatch credential $ \case
        PPubKeyCredential x -> pfromData $ pfield @"_0" # x
        _ -> perror

pTxSignedBy :: Term s (PTxInfo :--> PPubKeyHash :--> PBool)
pTxSignedBy = plam $ \txInfo rpkh -> unTermCont $ do
  signatories <- tletField @"signatories" txInfo
  pure (pelem # pdata rpkh # signatories)