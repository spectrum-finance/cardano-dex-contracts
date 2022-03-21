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
import PExtra.List

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
redeemValidatorT = plam $ \cfg' redeemer' ctx' -> unTermCont $ do
  txInfo'  <- tletField @"txInfo" ctx'
  txInfo   <- tcont $ pletFields @'["inputs", "outputs", "signatories"] txInfo'
  inputs   <- tletUnwrap $ hrecField @"inputs" txInfo
  outputs  <- tletUnwrap $ hrecField @"outputs" txInfo
  redeemer <- tcont $ pletFields @'["poolInIx", "orderInIx", "rewardOutIx", "action"] redeemer'
  cfg      <- tcont $ pletFields @'["exFee", "poolNft", "poolX", "poolY", "poolLq", "rewardPkh"] cfg'

  orderInIx   <- tletUnwrap $ hrecField @"orderInIx" redeemer
  poolInIx    <- tletUnwrap $ hrecField @"poolInIx" redeemer
  rewardOutIx <- tletUnwrap $ hrecField @"rewardOutIx" redeemer

  rewardPkh   <- tletUnwrap $ hrecField @"rewardPkh" cfg
  rewardOut   <- tlet $ pelemAt # rewardOutIx # outputs
  rewardValue <- tlet $ getRewardValue' # rewardOut # rewardPkh

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
  
  outs      <- tlet $ calcOut # rewardValue # poolX # poolY # collateralAda
  inLq      <- tlet $ assetClassValueOf # orderValue # poolLq 
  liquidity <- tlet $ maxLqCap - assetClassValueOf # poolValue # poolLq

  let
    outAda     = pGetLovelace # rewardValue
    minReturnX = calcMinReturn # liquidity # inLq # poolValue # poolX
    minReturnY = calcMinReturn # liquidity # inLq # poolValue # poolY

    outX  = pfromData $ pfield @"_0" # outs
    outY  = pfromData $ pfield @"_1" # outs
    opAda = pfromData $ pfield @"_2" # outs

    fairShare  = minReturnX #< outX #&& minReturnY #< outY
    fairFee    = opAda + collateralAda #< outAda

  action <- tletUnwrap $ hrecField @"action" redeemer
  pure $ pmatch action $ \case
    Apply  -> validPoolNft #&& validInputs #&& fairShare #&& fairFee
    Refund -> let sigs = pfromData $ hrecField @"signatories" txInfo
              in containsSignature # sigs # rewardPkh

calcMinReturn :: Term s (PInteger :--> PInteger :--> PValue :--> PAssetClass :--> PInteger)
calcMinReturn =
  phoistAcyclic $
    plam $ \liquidity inLq poolValue ac ->
      let reservesX = assetClassValueOf # poolValue # ac
      in pdiv # (inLq * reservesX) # liquidity

calcOut :: Term s (PValue :--> PAssetClass :--> PAssetClass :--> PInteger :--> PTuple3 PInteger PInteger PInteger)
calcOut = plam $ \rewardValue poolX poolY collateralAda -> unTermCont $ do
  rx <- tlet $ assetClassValueOf # rewardValue # poolX
  ry <- tlet $ assetClassValueOf # rewardValue # poolY

  outX <- tlet $ rx - collateralAda
  outY <- tlet $ ry - collateralAda

  let
    ifX    = ptuple3 # pdata outX # pdata ry # pdata outX
    ifY    = ptuple3 # pdata rx # pdata outY # pdata outY
    ifElse = ptuple3 # pdata rx # pdata ry # zeroAsData
  pure $ pif (pIsAda # poolX) ifX (pif (pIsAda # poolY) ifY ifElse)
