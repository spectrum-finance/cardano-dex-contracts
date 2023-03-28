{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.PContracts.PRedeem (
    RedeemConfig (..),
    redeemValidatorT,
) where

import qualified GHC.Generics  as GHC

import Plutarch
import Plutarch.Api.V2
import Plutarch.Api.V1.Value
import Plutarch.DataRepr
import Plutarch.Lift
import Plutarch.Prelude
import Plutarch.Extra.TermCont

import PExtra.API
import PExtra.Ada     (pIsAda)
import PExtra.Monadic (tlet, tmatch)
import PExtra.PTriple (PTuple3, ptuple3)

import ErgoDex.PContracts.PApi   (containsSignature, getRewardValue', maxLqCap, zeroAsData)
import ErgoDex.PContracts.POrder (OrderAction (Apply, Refund), OrderRedeemer)

import qualified ErgoDex.Contracts.Proxy.Redeem as R

newtype RedeemConfig (s :: S)
    = RedeemConfig
        ( Term
            s
            ( PDataRecord
                '[ "poolNft" ':= PAssetClass
                 , "x" ':= PAssetClass
                 , "y" ':= PAssetClass
                 , "lq" ':= PAssetClass
                 , "exFee" ':= PInteger
                 , "rewardPkh" ':= PPubKeyHash
                 , "stakePkh" ':= PMaybeData PPubKeyHash
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType RedeemConfig where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl RedeemConfig where type PLifted RedeemConfig = R.RedeemConfig
deriving via (DerivePConstantViaData R.RedeemConfig RedeemConfig) instance (PConstantDecl R.RedeemConfig)

redeemValidatorT :: ClosedTerm (RedeemConfig :--> OrderRedeemer :--> PScriptContext :--> PBool)
redeemValidatorT = plam $ \conf' redeemer' ctx' -> unTermCont $ do
    ctx     <- pletFieldsC @'["txInfo", "purpose"] ctx'
    conf    <- pletFieldsC @'["x", "y", "lq", "poolNft", "exFee", "rewardPkh", "stakePkh"] conf'
    let
        rewardPkh = getField @"rewardPkh" conf
        stakePkh  = getField @"stakePkh"  conf

        x  = getField @"x"  conf
        y  = getField @"y"  conf
        lq = getField @"lq" conf

        exFee   = getField @"exFee"  conf
        txInfo' = getField @"txInfo" ctx

    txInfo  <- pletFieldsC @'["inputs", "outputs", "signatories"] txInfo'
    inputs  <- tletUnwrap $ getField @"inputs"  txInfo
    outputs <- tletUnwrap $ getField @"outputs" txInfo

    redeemer <- pletFieldsC @'["poolInIx", "orderInIx", "rewardOutIx", "action"] redeemer'
    let
        poolInIx    = getField @"poolInIx" redeemer
        orderInIx   = getField @"orderInIx" redeemer
        rewardOutIx = getField @"rewardOutIx" redeemer

    rewardOut   <- tlet $ pelemAt # rewardOutIx # outputs
    rewardValue <- tlet $ getRewardValue' # rewardOut # rewardPkh # stakePkh

    poolIn'   <- tlet $ pelemAt # poolInIx # inputs
    poolIn    <- pletFieldsC @'["outRef", "resolved"] poolIn'
    let 
        poolValue    = pfield @"value" # (getField @"resolved" poolIn)
        poolIdentity = -- operation is performed with the pool selected by the user 
            let 
                requiredNft = pfromData $ getField @"poolNft" conf
                nftAmount   = assetClassValueOf # poolValue # requiredNft
             in nftAmount #== 1

    selfIn' <- tlet $ pelemAt # orderInIx # inputs
    selfIn  <- pletFieldsC @'["outRef", "resolved"] selfIn'
    let selfValue = pfield @"value" # (getField @"resolved" selfIn)

    PSpending selfRef' <- tmatch (pfromData $ getField @"purpose" ctx)
    let selfIdentity =
            let 
                selfRef = pfromData $ pfield @"_0" # selfRef'
                selfInRef = pfromData $ getField @"outRef" selfIn
             in selfRef #== selfInRef

    collateralAda <- -- we reserve a small amount of ADA to put it into user output later
        let inAda = plovelaceValueOf # selfValue
         in tlet $ inAda - exFee

    let strictInputs = -- ensure double satisfaction attack is not possible
        let inputsLength = plength # inputs
            in inputsLength #== 2

    liquidity <-
        let lqNegative = assetClassValueOf # poolValue # lq
         in tlet $ maxLqCap - lqNegative

    outs <- tlet $ calcOutput # rewardValue # x # y # collateralAda
    inLq <- tlet $ assetClassValueOf # selfValue # lq

    let 
        outAda = plovelaceValueOf # rewardValue
        
        minReturnX = calcMinReturn # liquidity # inLq # poolValue # x
        minReturnY = calcMinReturn # liquidity # inLq # poolValue # y

        outX  = pfromData $ pfield @"_0" # outs
        outY  = pfromData $ pfield @"_1" # outs
        opAda = pfromData $ pfield @"_2" # outs

        fairShare = minReturnX #<= outX #&& minReturnY #<= outY -- output shares are proportional to the total LQ and LQ returned by the user
        fairFee = opAda + collateralAda #<= outAda -- output ADA (if present) plus collateral ADA is returned in full to the user

    action <- tletUnwrap $ getField @"action" redeemer
    pure $
        pmatch action $ \case
            Apply -> poolIdentity #&& selfIdentity #&& strictInputs #&& fairShare #&& fairFee
            Refund ->
                let sigs = pfromData $ getField @"signatories" txInfo
                 in containsSignature # sigs # rewardPkh -- user signed the refund

calcMinReturn :: Term s (PInteger :--> PInteger :--> PValue _ _:--> PAssetClass :--> PInteger)
calcMinReturn =
    phoistAcyclic $
        plam $ \liquidity inLq poolValue ac ->
            let reserves = assetClassValueOf # poolValue # ac
             in pdiv # (inLq * reserves) # liquidity

calcOutput :: Term s (PValue _ _:--> PAssetClass :--> PAssetClass :--> PInteger :--> PTuple3 PInteger PInteger PInteger)
calcOutput = plam $ \rewardValue poolX poolY collateralAda -> unTermCont $ do
    rx <- tlet $ assetClassValueOf # rewardValue # poolX
    ry <- tlet $ assetClassValueOf # rewardValue # poolY

    outX <- tlet $ rx - collateralAda
    outY <- tlet $ ry - collateralAda

    let ifX = ptuple3 # pdata outX # pdata ry # pdata outX
        ifY = ptuple3 # pdata rx # pdata outY # pdata outY
        ifElse = ptuple3 # pdata rx # pdata ry # zeroAsData
    pure $ pif (pIsAda # poolX) ifX (pif (pIsAda # poolY) ifY ifElse)
