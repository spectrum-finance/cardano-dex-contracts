{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.PContracts.PRedeem
  ( RedeemConfig(..)
  , redeemValidatorT
  ) where

import qualified GHC.Generics as GHC
import           Generics.SOP (Generic, I (I))

import Plutarch
import Plutarch.Prelude
import Plutarch.DataRepr
import Plutarch.Api.V1.Contexts
import Plutarch.Api.V1
import Plutarch.Lift

import PExtra.API
import PExtra.Monadic (tlet, tletField, tmatch)
import PExtra.Ada     ( pIsAda, pGetLovelace )

import ErgoDex.PContracts.PApi
    ( zeroAsData,
      tletUnwrap,
      containsSignature,
      getRewardValue',
      maxLqCap )
import ErgoDex.PContracts.POrder ( OrderRedeemer, OrderAction(Refund, Apply) )
import PExtra.PTriple            ( ptuple3, PTuple3 )
import PExtra.List               ( pelemAt )

import qualified ErgoDex.Contracts.Proxy.Redeem as R

newtype RedeemConfig (s :: S) = RedeemConfig
  (
    Term s (
      PDataRecord
        '[ "poolNft"   ':= PAssetClass
         , "x"         ':= PAssetClass
         , "y"         ':= PAssetClass
         , "lq"        ':= PAssetClass
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

instance PUnsafeLiftDecl RedeemConfig where type PLifted RedeemConfig = R.RedeemConfig
deriving via (DerivePConstantViaData R.RedeemConfig RedeemConfig) instance (PConstant R.RedeemConfig)

redeemValidatorT :: ClosedTerm (RedeemConfig :--> OrderRedeemer :--> PScriptContext :--> PBool)
redeemValidatorT = plam $ \conf' redeemer' ctx' -> unTermCont $ do
  ctx     <- tcont $ pletFields @'["txInfo", "purpose"] ctx'
  conf    <- tcont $ pletFields @'["x", "y", "lq", "poolNft", "exFee", "rewardPkh"] conf'
  txInfo' <- tletUnwrap $ hrecField @"txInfo" ctx
  txInfo  <- tcont $ pletFields @'["inputs", "outputs", "signatories"] txInfo'
  inputs  <- tletUnwrap $ hrecField @"inputs" txInfo
  outputs <- tletUnwrap $ hrecField @"outputs" txInfo

  redeemer    <- tcont $ pletFields @'["poolInIx", "orderInIx", "rewardOutIx", "action"] redeemer'
  poolInIx    <- tletUnwrap $ hrecField @"poolInIx" redeemer
  orderInIx   <- tletUnwrap $ hrecField @"orderInIx" redeemer
  rewardOutIx <- tletUnwrap $ hrecField @"rewardOutIx" redeemer

  rewardOut   <- tlet $ pelemAt # rewardOutIx # outputs
  rewardPkh   <- tletUnwrap $ hrecField @"rewardPkh" conf
  rewardValue <- tlet $ getRewardValue' # rewardOut # rewardPkh

  poolIn'   <- tlet $ pelemAt # poolInIx # inputs
  poolIn    <- tcont $ pletFields @'["outRef", "resolved"] poolIn'
  poolValue <-
    let pool = pfromData $ hrecField @"resolved" poolIn
    in tletField @"value" pool
  let
    poolIdentity =
      let
        requiredNft = pfromData $ hrecField @"poolNft" conf
        nftAmount   = assetClassValueOf # poolValue # requiredNft
      in nftAmount #== 1

  selfIn'   <- tlet $ pelemAt # orderInIx # inputs
  selfIn    <- tcont $ pletFields @'["outRef", "resolved"] selfIn'
  selfValue <-
    let self = pfromData $ hrecField @"resolved" selfIn
    in tletField @"value" self

  PSpending selfRef' <- tmatch (pfromData $ hrecField @"purpose" ctx)
  let
    selfIdentity =
      let
        selfRef   = pfromData $ pfield @"_0" # selfRef'
        selfInRef = pfromData $ hrecField @"outRef" selfIn
      in selfRef #== selfInRef

  x  <- tletUnwrap $ hrecField @"x" conf
  y  <- tletUnwrap $ hrecField @"y" conf
  lq <- tletUnwrap $ hrecField @"lq" conf

  exFee         <- tletUnwrap $ hrecField @"exFee" conf
  collateralAda <-
    let inAda = pGetLovelace # selfValue
    in tlet $ inAda - exFee

  let
    strictInputs =
      let inputsLength = plength # inputs
      in inputsLength #== 2

  liquidity <-
    let lqNegative = assetClassValueOf # poolValue # lq
    in tlet $ maxLqCap - lqNegative

  outs <- tlet $ calcOutput # rewardValue # x # y # collateralAda
  inLq <- tlet $ assetClassValueOf # selfValue # lq 

  let
    outAda     = pGetLovelace # rewardValue
    minReturnX = calcMinReturn # liquidity # inLq # poolValue # x
    minReturnY = calcMinReturn # liquidity # inLq # poolValue # y

    outX  = pfromData $ pfield @"_0" # outs
    outY  = pfromData $ pfield @"_1" # outs
    opAda = pfromData $ pfield @"_2" # outs

    fairShare = minReturnX #<= outX #&& minReturnY #<= outY
    fairFee   = opAda + collateralAda #<= outAda

  action <- tletUnwrap $ hrecField @"action" redeemer
  pure $ pmatch action $ \case
    Apply  -> poolIdentity #&& selfIdentity #&& strictInputs #&& fairShare #&& fairFee
    Refund -> let sigs = pfromData $ hrecField @"signatories" txInfo
              in containsSignature # sigs # rewardPkh

calcMinReturn :: Term s (PInteger :--> PInteger :--> PValue :--> PAssetClass :--> PInteger)
calcMinReturn =
  phoistAcyclic $
    plam $ \liquidity inLq poolValue ac ->
      let reservesX = assetClassValueOf # poolValue # ac
      in pdiv # (inLq * reservesX) # liquidity

calcOutput :: Term s (PValue :--> PAssetClass :--> PAssetClass :--> PInteger :--> PTuple3 PInteger PInteger PInteger)
calcOutput = plam $ \rewardValue poolX poolY collateralAda -> unTermCont $ do
  rx <- tlet $ assetClassValueOf # rewardValue # poolX
  ry <- tlet $ assetClassValueOf # rewardValue # poolY

  outX <- tlet $ rx - collateralAda
  outY <- tlet $ ry - collateralAda

  let
    ifX    = ptuple3 # pdata outX # pdata ry # pdata outX
    ifY    = ptuple3 # pdata rx # pdata outY # pdata outY
    ifElse = ptuple3 # pdata rx # pdata ry # zeroAsData
  pure $ pif (pIsAda # poolX) ifX (pif (pIsAda # poolY) ifY ifElse)
