{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.PContracts.LqMining.Simple.PStakingBundle where

import qualified GHC.Generics  as GHC

import qualified Plutarch.Api.V1.Value  as V1

import Plutarch
import Plutarch.Api.V2              (PTxOut, POutputDatum(POutputDatum), PPubKeyHash, PDatum(PDatum), PTxInInfo)
import Plutarch.Api.V2.Contexts     (PScriptContext, PScriptPurpose (PSpending))
import Plutarch.DataRepr
import Plutarch.Lift
import Plutarch.Prelude
import Plutarch.Extra.TermCont

import PExtra.API                   (PAssetClass, assetClassValueOf, ptryFromData)
import PExtra.Monadic               (tlet, tletField, tmatch)

import ErgoDex.PContracts.PPool
import ErgoDex.PContracts.PApi      (maxLqCap, tletUnwrap, getRewardValueByPkh')

import qualified ErgoDex.Contracts.Proxy.LqMining.Simple.StakingBundle as SB
import qualified ErgoDex.PContracts.LqMining.Simple.PLMPool as Pool

newtype StakingBundleConfig (s :: S)
    = StakingBundleConfig
        ( Term
            s
            ( PDataRecord
                '[ "bundleAC"    ':= PAssetClass
                 , "poolAC"      ':= PAssetClass
                 , "bundleLQAC"  ':= PAssetClass
                 , "bundleVLQAC" ':= PAssetClass
                 , "bundleTMPAC" ':= PAssetClass
                 , "redeemerPkh" ':= PPubKeyHash
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PlutusType, PIsData, PDataFields, PEq, PShow)

instance DerivePlutusType StakingBundleConfig where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl StakingBundleConfig where type PLifted StakingBundleConfig = SB.StakingBundleConfig
deriving via (DerivePConstantViaData SB.StakingBundleConfig StakingBundleConfig) instance (PConstantDecl SB.StakingBundleConfig)

instance PTryFrom PData (PAsData StakingBundleConfig)

newtype StakingBundleRedeemer (s :: S)
    = StakingBundleRedeemer
        ( Term
            s
            ( PDataRecord
                '[ "poolInIdx"  ':= PInteger
                 , "permitIdx"  ':= PInteger
                 , "selfInIdx"  ':= PInteger
                 , "redeemerOutIx" ':= PInteger
                 , "successorOutIndex" ':= PInteger
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType StakingBundleRedeemer where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl StakingBundleRedeemer where type PLifted StakingBundleRedeemer = SB.StakingBundleRedeemer
deriving via (DerivePConstantViaData SB.StakingBundleRedeemer StakingBundleRedeemer) instance (PConstantDecl SB.StakingBundleRedeemer)

stakingBundleValidatorT :: ClosedTerm (StakingBundleConfig :--> StakingBundleRedeemer :--> PScriptContext :--> PBool)
stakingBundleValidatorT = plam $ \conf' redeemer' ctx' -> unTermCont $ do
    ctx      <- pletFieldsC @'["txInfo", "purpose"] ctx'
    conf     <- pletFieldsC @'["bundleAC", "poolAC", "redeemerPkh", "bundleVLQAC", "bundleTMPAC", "bundleLQAC"] conf'
    redeemer <- pletFieldsC @'["poolInIdx", "permitIdx", "selfInIdx", "redeemerOutIx", "successorOutIndex"] redeemer'
    txInfo   <- pletFieldsC @'["inputs", "outputs"] $ getField @"txInfo" ctx

    inputs   <- tletUnwrap $ getField @"inputs"  txInfo
    outputs  <- tletUnwrap $ getField @"outputs" txInfo
    let
        bundleAC    = getField @"bundleAC"    conf
        poolNFT     = getField @"poolAC"      conf
        redeemerPkh = getField @"redeemerPkh" conf
        bundleVLQAC = getField @"bundleVLQAC" conf
        bundleTMPAC = getField @"bundleTMPAC" conf
        bundleLQAC  = getField @"bundleLQAC" conf

        poolInIdx   = getField @"poolInIdx"  redeemer
        permitIdx   = getField @"permitIdx" redeemer
        selfInIdx   = getField @"selfInIdx" redeemer

        purpose = pfromData $ getField @"purpose" ctx

        redeemerOutIx     = getField @"redeemerOutIx" redeemer
        successorOutIndex = getField @"successorOutIndex" redeemer

    poolIn' <- tlet $ pelemAt # poolInIdx # inputs
    let pool = pfield @"resolved" # poolIn'

    poolInValue <- tletField @"value" pool
    poolInDatum <- tletField @"datum" pool

    POutputDatum poolInD' <- pmatchC poolInDatum

    poolInD <- tletField @"outputDatum" poolInD'

    PDatum succPoolInDatum <- pmatchC poolInD

    succPoolInDatum' <- tletUnwrap $ ptryFromData @(Pool.LMPoolConfig) $ succPoolInDatum

    poolOut <- tlet $ findPoolOutput # poolNFT # outputs
    let
        poolOutValue = pfield @"value" # poolOut

        poolLQ    = pfield @"poolLQ" # succPoolInDatum'
        poolOutLQ = assetClassValueOf # poolOutValue # poolLQ
        lqLockedInPoolTotal = assetClassValueOf # poolInValue # poolLQ

        deltaLQ = poolOutLQ - lqLockedInPoolTotal

    validAction <-
        tlet $
            pif
                (deltaLQ #== 0) -- compound
                (checkCompound 
                    # succPoolInDatum' 
                    # poolInValue
                    # purpose
                    # poolNFT
                    # bundleVLQAC
                    # bundleTMPAC
                    # poolLQ
                    # inputs 
                    # selfInIdx 
                    # outputs 
                    # successorOutIndex 
                    # conf'
                    # redeemerOutIx
                    # redeemerPkh
                )
                (
                    pif
                        (deltaLQ #< 0) -- redeem
                        (checkRedeem # inputs # permitIdx # bundleLQAC)
                        (pcon PFalse)
                )
    pure $ validAction

checkCompound :: 
    Term 
        s 
        ( Pool.LMPoolConfig 
            :--> V1.PValue 'V1.Sorted 'V1.Positive
            :--> PScriptPurpose
            :--> PAssetClass
            :--> PAssetClass
            :--> PAssetClass
            :--> PAssetClass
            :--> PBuiltinList PTxInInfo
            :--> PInteger
            :--> PBuiltinList PTxOut
            :--> PInteger
            :--> StakingBundleConfig
            :--> PInteger
            :--> PPubKeyHash
            :--> PBool
        )
checkCompound = 
    plam$ \inputPoolCfg inputPoolValue purpose poolNFT bundleVLQAC bundleTMPAC poolLQ inputs selfIdx outputs successorIdx selfCfg redeemerIdx rewardPkh -> unTermCont $ do
        poolOut <- tlet $ findPoolOutput # poolNFT # outputs
    
        poolOutDatum <- tletField @"datum" poolOut
    
        POutputDatum poolD' <- pmatchC poolOutDatum
    
        poolD <- tletField @"outputDatum" poolD'
    
        PDatum succDatum  <- pmatchC poolD
        succPoolOutDatum' <- tletUnwrap $ ptryFromData @(Pool.LMPoolConfig) $ succDatum
    
        outputPoolCfg <- pletFieldsC @'["programBudget", "epoch", "poolX"] succPoolOutDatum'
        let
            programBudget = getField @"programBudget" outputPoolCfg
            poolX  = getField @"poolX"  outputPoolCfg
    
            lqLockedInPoolTotal = assetClassValueOf # inputPoolValue # poolLQ
    
            epoch = getField @"epoch"    outputPoolCfg
    
            epochNum = pfield @"epochNum" # inputPoolCfg
    
            epochRewardTotal = pdiv # programBudget # epochNum
            epochsToCompound = epochNum - epoch
    
        selfIn'   <- tlet $ pelemAt # selfIdx # inputs
        selfIn    <- pletFieldsC @'["outRef", "resolved"] selfIn'
        let self = getField @"resolved" selfIn
    
        selfValue <- tletField @"value" self
        selfDatum <- tletField @"datum" self
    
        PSpending selfRef' <- tmatch purpose
        let 
            selfIdentity =
                let selfRef   = pfromData $ pfield @"_0" # selfRef'
                    selfInRef = pfromData $ getField @"outRef" selfIn
                 in selfRef #== selfInRef
        
        successorOut' <- tlet $ pelemAt # successorIdx # outputs
        successorOut  <- pletFieldsC @'["value", "datum"] successorOut'
        redeemerOut   <- tlet $ pelemAt # redeemerIdx # outputs
        let
            redeemerValue = getRewardValueByPkh' # redeemerOut # rewardPkh
    
            rewardX = assetClassValueOf # redeemerValue # poolX
    
            successorValue = getField @"value" successorOut
            successorDatum = getField @"datum" successorOut
    
            bundleVLQQty = assetClassValueOf # selfValue # bundleVLQAC
            bundleTMPQty = assetClassValueOf # selfValue # bundleTMPAC
            releasedTMP  = bundleTMPQty - epochsToCompound * bundleVLQQty
    
            successorBundleVLQ = assetClassValueOf # successorValue # bundleVLQAC
            successorBundleTMP = assetClassValueOf # successorValue # bundleTMPAC
    
            epcohsBurned = (pdiv # bundleTMPQty # bundleVLQQty) - epochsToCompound
            reward = pdiv # (epochRewardTotal * bundleVLQQty * epcohsBurned) # lqLockedInPoolTotal
    
            correctDatum  = selfDatum #== successorDatum
            correctVLQQty = successorBundleVLQ #== bundleVLQQty
            correctTMPQty = (bundleTMPQty - successorBundleTMP) #== releasedTMP
            correctReward = reward #== rewardX

        _ <- tlet $ ptraceShowId reward
    
        pure $ selfIdentity #&& correctDatum #&& correctVLQQty #&& correctTMPQty #&& correctReward

checkRedeem :: Term s (PBuiltinList PTxInInfo :--> PInteger :--> PAssetClass :--> PBool)        
checkRedeem =
    plam $ \inputs permitIdx bundleAC -> unTermCont $ do
        permitIn' <- tlet $ pelemAt # permitIdx # inputs
        let
            permitResolved = pfield @"resolved" # permitIn'
            permitInValue  = pfield @"value" # permitResolved
            permitACValue  = assetClassValueOf # permitInValue # bundleAC

            correctpermitACValue = permitACValue #== maxLqCap

        pure correctpermitACValue