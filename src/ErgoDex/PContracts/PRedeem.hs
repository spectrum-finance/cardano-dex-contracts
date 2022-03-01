{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.PContracts.PRedeem where

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
import PExtra.PTriple

newtype PRedeemRedeemer (s :: S) = PSwapRedeemer
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
    via (PIsDataReprInstances PRedeemRedeemer)

newtype PRedeemConfig (s :: S) = PSwapConfig
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
    via (PIsDataReprInstances PRedeemConfig)

pmkRedeemValidator :: Term s (PRedeemConfig :--> PRedeemRedeemer :--> PScriptContext :--> PBool)
pmkRedeemValidator = plam $ \configT redeemerT cxtT -> unTermCont $ do
  txInfo    <- tletField @"txInfo" cxtT
  rewardPkh <- tletField @"rewardPkh" configT

  isTxSignedByRewardKey <- tlet $ hasValidSignatories # txInfo # rewardPkh

  return $ isTxSignedByRewardKey #|| (isValidRedeem # txInfo # rewardPkh # redeemerT # configT)

isValidRedeem :: Term s (PTxInfo :--> PPubKeyHash :--> PRedeemRedeemer :--> PRedeemConfig :--> PBool)
isValidRedeem = plam $ \txInfo rewardPkh redeemerT configT -> unTermCont $ do
  indexes <- tcont $ pletFields @'["orderIndex", "poolIndex", "rewardIndex"] redeemerT
  cfg     <- tcont $ pletFields @'["exFee", "poolNft"] configT

  orderIndex  <- tletUnwrap $ hrecField @"orderIndex" indexes
  poolIndex   <- tletUnwrap $ hrecField @"poolIndex" indexes
  rewardIndex <- tletUnwrap $ hrecField @"rewardIndex" indexes

  rewardValue <- tlet $ getRewardValue # txInfo # rewardIndex # rewardPkh

  inputs     <- tletField @"inputs" txInfo
  poolValue  <- tlet $ getInputValue # inputs # poolIndex
  orderValue <- tlet $ getInputValue # inputs # orderIndex

  poolNft <- tletUnwrap $ hrecField @"poolNft" cfg

  _ <- tlet $ poolCheckNft # poolValue # poolNft
  _ <- tlet $ validInputsQty # inputs

  return $ isFair # configT # poolValue # rewardValue # orderValue

isFair :: Term s (PRedeemConfig :--> PValue :--> PValue :--> PValue :--> PBool)
isFair = plam $ \configT poolValue rewardValue orderValue -> unTermCont $ do
  cfg    <- tcont $ pletFields @'["exFee", "poolX", "poolY", "poolLq"] configT
  poolX  <- tletUnwrap $ hrecField @"poolX" cfg
  poolY  <- tletUnwrap $ hrecField @"poolY" cfg
  poolLq <- tletUnwrap $ hrecField @"poolLq" cfg

  inAda         <- tlet $ pGetLovelace # orderValue
  exFee         <- tletUnwrap $ hrecField @"exFee" cfg
  collateralAda <- tlet $ inAda - exFee

  outs <- tlet $ calcOut # rewardValue # poolX # poolY # collateralAda

  outX  <- tlet $ pfromData $ pfield @"_0" # outs
  outY  <- tlet $ pfromData $ pfield @"_1" # outs
  opAda <- tlet $ pfromData $ pfield @"_2" # outs

  outAda <- tlet $ pGetLovelace # rewardValue

  inLq <- tlet $ assetClassValueOf # orderValue # poolLq 

  liquidity  <- tlet $ maxLqCap - assetClassValueOf # poolValue # poolLq

  minReturnX <- tlet $ calcMinReturn # liquidity # inLq # poolValue # poolX
  minReturnY <- tlet $ calcMinReturn # liquidity # inLq # poolValue # poolY

  fairShare <- tlet $ minReturnX #< outX #&& minReturnY #< outY
  fairFee   <- tlet $ opAda + collateralAda #< outAda

  return $ fairShare #&& fairFee

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
  ifelse <- tlet $ ptuple3 # pdata rx # pdata ry # pdata 0

  pure $ pif (pIsAda # poolX) ifX (pif (pIsAda # poolY) ifY ifelse)
    