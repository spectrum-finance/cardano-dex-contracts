{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.PContracts.PRedeem
  ( RedeemConfig(..)
  , redeemValidatorT
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
import PExtra.Ada

import ErgoDex.PContracts.PApi
import ErgoDex.PContracts.POrder
import PExtra.PTriple

newtype RedeemConfig (s :: S) = RedeemConfig
  (
    Term s (
      PDataRecord
        '[ "poolNft"   ':= PAssetClass
         , "poolX"     ':= PAssetClass
         , "poolY"     ':= PAssetClass
         , "poolLq"    ':= PAssetClass
         , "exFee"     ':= PInteger
         , "rewardPkh" ':= PPubKeyHash
        ]
    )
  )  
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PMatch, PIsData, PDataFields, PlutusType)
    via (PIsDataReprInstances RedeemConfig)

redeemValidatorT :: ClosedTerm (RedeemConfig :--> OrderRedeemer :--> PScriptContext :--> PBool)
redeemValidatorT = plam $ \configT redeemerT cxtT -> unTermCont $ do
  txInfo    <- tletField @"txInfo" cxtT
  rewardPkh <- tletField @"rewardPkh" configT

  isTxSignedByRewardKey <- tlet $ hasValidSignatories # txInfo # rewardPkh

  indexes <- tcont $ pletFields @'["orderInIx", "poolInIx", "rewardOutIx"] redeemerT
  cfg     <- tcont $ pletFields @'["exFee", "poolNft", "poolX", "poolY", "poolLq"] configT

  orderInIx   <- tletUnwrap $ hrecField @"orderInIx" indexes
  poolInIx    <- tletUnwrap $ hrecField @"poolInIx" indexes
  rewardOutIx <- tletUnwrap $ hrecField @"rewardOutIx" indexes

  rewardValue <- tlet $ getRewardValue # txInfo # rewardOutIx # rewardPkh

  inputs     <- tletField @"inputs" txInfo
  poolValue  <- tlet $ getInputValue # inputs # poolInIx
  orderValue <- tlet $ getInputValue # inputs # orderInIx

  poolNft <- tletUnwrap $ hrecField @"poolNft" cfg

  let
    validPoolNft = checkPoolNft # poolValue # poolNft
    validInputs  = checkInputsQty # inputs

  poolX  <- tletUnwrap $ hrecField @"poolX" cfg
  poolY  <- tletUnwrap $ hrecField @"poolY" cfg
  poolLq <- tletUnwrap $ hrecField @"poolLq" cfg

  let inAda = pGetLovelace # orderValue
  exFee         <- tletUnwrap $ hrecField @"exFee" cfg
  collateralAda <- tlet $ inAda - exFee

  outs <- tlet $ calcOut # rewardValue # poolX # poolY # collateralAda

  inLq       <- tlet $ assetClassValueOf # orderValue # poolLq 
  liquidity  <- tlet $ maxLqCap - assetClassValueOf # poolValue # poolLq

  let
    outAda     = pGetLovelace # rewardValue
    minReturnX = calcMinReturn # liquidity # inLq # poolValue # poolX
    minReturnY = calcMinReturn # liquidity # inLq # poolValue # poolY

    outX  = pfromData $ pfield @"_0" # outs
    outY  = pfromData $ pfield @"_1" # outs
    opAda = pfromData $ pfield @"_2" # outs

    fairShare  = minReturnX #< outX #&& minReturnY #< outY
    fairFee    = opAda + collateralAda #< outAda

    validRedeem = validPoolNft #&& validInputs #&& fairShare #&& fairFee

  return $ isTxSignedByRewardKey #|| validRedeem

calcMinReturn :: Term s (PInteger :--> PInteger :--> PValue :--> PAssetClass :--> PInteger)
calcMinReturn = plam $ \liquidity inLq poolValue ac -> unTermCont $ do
  reservesX  <- tlet $ assetClassValueOf # poolValue # ac
  return $ pdiv # (inLq * reservesX) # liquidity

calcOut :: Term s (PValue :--> PAssetClass :--> PAssetClass :--> PInteger :--> PTuple3 PInteger PInteger PInteger)
calcOut = plam $ \rewardValue poolX poolY collateralAda -> unTermCont $ do
  rx <- tlet $ assetClassValueOf # rewardValue # poolX
  ry <- tlet $ assetClassValueOf # rewardValue # poolY

  outX <- tlet $ rx - collateralAda
  outY <- tlet $ ry - collateralAda

  ifX    <- tlet $ ptuple3 # pdata outX # pdata ry # pdata outX
  ifY    <- tlet $ ptuple3 # pdata rx # pdata outY # pdata outY
  ifElse <- tlet $ ptuple3 # pdata rx # pdata ry # zeroAsData

  pure $ pif (pIsAda # poolX) ifX (pif (pIsAda # poolY) ifY ifElse)
    