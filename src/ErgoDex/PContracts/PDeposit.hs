{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ErgoDex.PContracts.PDeposit
  ( DepositConfig(..)
  , depositValidatorT
  ) where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic, I (I))

import Plutarch
import Plutarch.Prelude
import Plutarch.DataRepr
import Plutarch.Api.V1.Contexts

import PExtra.API
import PExtra.Ada
import Plutarch.Api.V1 (PPubKeyHash, PValue)
import PExtra.Monadic  (tlet, tmatch, tletField)
import PExtra.List     (pelemAt)

import ErgoDex.PContracts.PApi
import ErgoDex.PContracts.POrder

newtype DepositConfig (s :: S) = DepositConfig
  (
    Term s (
      PDataRecord
      '[ "poolNft"       ':= PAssetClass
       , "x"             ':= PAssetClass
       , "y"             ':= PAssetClass
       , "lq"            ':= PAssetClass
       , "exFee"         ':= PInteger
       , "rewardPkh"     ':= PPubKeyHash
       , "collateralAda" ':= PInteger
       ]
    )
  )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PMatch, PIsData, PDataFields, PlutusType)
    via (PIsDataReprInstances DepositConfig)

depositValidatorT :: ClosedTerm (DepositConfig :--> OrderRedeemer :--> PScriptContext :--> PBool)
depositValidatorT = plam $ \datum' redeemer' ctx' -> unTermCont $ do
  ctx       <- tcont $ pletFields @'["txInfo", "purpose"] ctx'
  datum     <- tcont $ pletFields @'["x", "y", "lq", "poolNft", "exFee", "rewardPkh", "collateralAda"] datum'
  txInfo'   <- tletUnwrap $ hrecField @"txInfo" ctx
  txInfo    <- tcont $ pletFields @'["inputs", "outputs", "signatories"] txInfo'
  inputs    <- tletUnwrap $ hrecField @"inputs" txInfo
  outputs   <- tletUnwrap $ hrecField @"outputs" txInfo

  redeemer    <- tcont $ pletFields @'["poolInIx", "orderInIx", "rewardOutIx"] redeemer'
  poolInIx    <- tletUnwrap $ hrecField @"poolInIx" redeemer
  orderInIx   <- tletUnwrap $ hrecField @"orderInIx" redeemer
  rewardOutIx <- tletUnwrap $ hrecField @"rewardOutIx" redeemer

  rewardOut   <- tlet $ pelemAt # rewardOutIx # outputs
  rewardPkh   <- tletUnwrap $ hrecField @"rewardPkh" datum
  rewardValue <- tlet $ getRewardValue' # rewardOut # rewardPkh

  poolIn'   <- tlet $ pelemAt # poolInIx # inputs
  poolIn    <- tcont $ pletFields @'["outRef", "resolved"] poolIn'
  poolValue <-
    let pool = pfromData $ hrecField @"resolved" poolIn
    in tletField @"value" pool
  let
    poolIdentity =
      let
        requiredNft = pfromData $ hrecField @"poolNft" datum
        nftAmount   = assetClassValueOf # poolValue # requiredNft
      in pif (nftAmount #== 1) (pcon PTrue) (pcon PFalse) 

  selfIn'   <- tlet $ pelemAt # orderInIx # inputs
  selfIn    <- tcont $ pletFields @'["outRef", "resolved"] selfIn'
  selfValue <-
    let self = pfromData $ hrecField @"resolved" poolIn
    in tletField @"value" self

  PSpending selfRef' <- tmatch (pfromData $ hrecField @"purpose" ctx)
  let
    selfIdentity =
      let
        selfRef   = pfromData $ pfield @"_0" # selfRef'
        selfInRef = pfromData $ hrecField @"outRef" selfIn
      in selfRef #== selfInRef

  x  <- tletUnwrap $ hrecField @"x" datum
  y  <- tletUnwrap $ hrecField @"y" datum
  lq <- tletUnwrap $ hrecField @"lq" datum

  exFee         <- tletUnwrap $ hrecField @"exFee" datum
  collateralAda <- tletUnwrap $ hrecField @"collateralAda" datum

  let
    strictInputs =
      let inputsLength = plength # inputs
      in inputsLength #== 2
    fairFee =
      let outputAda = pGetLovelace # rewardValue
      in collateralAda #<= outputAda
    validReward =
      let
        lqNegative = assetClassValueOf # poolValue # lq
        liquidity  = maxLqCap - lqNegative
        minRewardByX = minAssetReward # selfValue # poolValue # x # liquidity # exFee # collateralAda
        minRewardByY = minAssetReward # selfValue # poolValue # y # liquidity # exFee # collateralAda
        minReward    = pmin # minRewardByX # minRewardByY -- todo: deposit slashing attack
        actualReward = assetClassValueOf # rewardValue # lq
      in minReward #<= actualReward
    
    validRefund  =
      let sigs = pfromData $ hrecField @"signatories" txInfo
      in hasValidSignatories' # sigs # rewardPkh
    validDeposit = poolIdentity #&& selfIdentity #&& strictInputs #&& fairFee #&& validReward
    
  pure $ validRefund #|| validDeposit

minAssetReward :: Term s (PValue :--> PValue :--> PAssetClass :--> PInteger :--> PInteger :--> PInteger :--> PInteger)
minAssetReward =
  phoistAcyclic $
    plam $ \selfValue poolValue asset liquidity exFee collateralAda ->
      unTermCont $ do
        assetInput <- tlet $ assetClassValueOf # selfValue # asset
        let
          inputDeposit = pif (pIsAda # asset)
            (assetInput - exFee - collateralAda)
            assetInput
          tokenReserves = assetClassValueOf # poolValue # asset
        pure $ pdiv # (inputDeposit * liquidity) # tokenReserves
