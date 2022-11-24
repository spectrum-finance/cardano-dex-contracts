{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.PContracts.LqMining.Simple.PLMPool where

import qualified GHC.Generics  as GHC

import Plutarch
import Plutarch.Api.V2              (PMaybeData (PDJust), PTxOut, POutputDatum(POutputDatum, PNoOutputDatum, POutputDatumHash), PPubKeyHash, PDatum(PDatum), PExtended(PFinite))
import Plutarch.Api.V2.Contexts     (PScriptContext, PScriptPurpose (PSpending))
import Plutarch.Api.V1.Time         (PPOSIXTime(PPOSIXTime))
import Plutarch.DataRepr
import Plutarch.Lift
import Plutarch.Prelude
import Plutarch.Extra.TermCont
import Plutarch.Builtin             (pasInt, pforgetData, pfromData, pdata, PIsData(..))
import Plutarch.Unsafe              (punsafeCoerce)
import Plutarch.Internal.PlutusType (PInner, PlutusType, pcon', pmatch')

import PExtra.Ada
import PExtra.API                   (PAssetClass, assetClassValueOf, ptryFromData)
import PExtra.List                  (pelemAt)
import PExtra.Monadic               (tcon, tlet, tletField, tmatch, tmatchField)

import qualified ErgoDex.Contracts.Pool  as P
import           ErgoDex.PContracts.PApi (burnLqInitial, feeDen, maxLqCap, tletUnwrap, zero, containsSignature, pmax)

import qualified ErgoDex.Contracts.Proxy.LqMining.Simple.LMPool as Pool

newtype LMPoolConfig (s :: S)
    = LMPoolConfig
        ( Term
            s
            ( PDataRecord
                '[ "epochLen"      ':= PInteger
                 , "epochNum"      ':= PInteger
                 , "programStart"  ':= PInteger
                 , "programBudget" ':= PInteger
                 , "execBudget"    ':= PInteger
                 , "epoch"         ':= PInteger
                 , "maxRoundingError" ':= PInteger
                 , "poolNft"       ':= PAssetClass
                 , "poolX"         ':= PAssetClass
                 , "poolLQ"        ':= PAssetClass
                 , "poolVLQ"       ':= PAssetClass
                 , "poolTMP"       ':= PAssetClass
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType LMPoolConfig where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl LMPoolConfig where type PLifted LMPoolConfig = Pool.LMPoolConfig
deriving via (DerivePConstantViaData Pool.LMPoolConfig LMPoolConfig) instance (PConstantDecl Pool.LMPoolConfig)

instance PTryFrom PData (PAsData LMPoolConfig)

newtype LMPoolRedeemer (s :: S)
    = LMPoolRedeemer
        ( Term
            s
            ( PDataRecord
                '[ "poolInIdx"  ':= PInteger
                 , "poolOutIdx" ':= PInteger
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType LMPoolRedeemer where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl LMPoolRedeemer where type PLifted LMPoolRedeemer = Pool.LMPoolRedeemer
deriving via (DerivePConstantViaData Pool.LMPoolRedeemer LMPoolRedeemer) instance (PConstantDecl Pool.LMPoolRedeemer)

lmPoolValidatorT :: ClosedTerm (LMPoolConfig :--> LMPoolRedeemer :--> PScriptContext :--> PBool)
lmPoolValidatorT = plam $ \conf' redeemer' ctx' -> unTermCont $ do
    ctx      <- pletFieldsC @'["txInfo", "purpose"] ctx'
    conf     <- pletFieldsC @'["epochLen", "epochNum", "programStart", "programBudget", "maxRoundingError", "execBudget", "poolNft", "poolX", "poolLQ", "poolVLQ", "poolTMP"] conf'
    redeemer <- pletFieldsC @'["poolInIdx", "poolOutIdx"] redeemer'
    txInfo   <- pletFieldsC @'["inputs", "outputs", "validRange"] $ getField @"txInfo" ctx

    validRange <- tletUnwrap $ getField @"validRange" txInfo
    inputs     <- tletUnwrap $ getField @"inputs"  txInfo
    outputs    <- tletUnwrap $ getField @"outputs" txInfo
    let
        epochLen      = getField @"epochLen"      conf
        epochNum      = getField @"epochNum"      conf
        programStart  = pfromData $ getField @"programStart" conf
        programBudget = getField @"programBudget" conf
        execBudget    = getField @"execBudget"    conf

        poolNft = getField @"poolNft" conf
        poolX   = getField @"poolX"   conf
        poolLQ  = getField @"poolLQ"  conf
        poolVLQ = getField @"poolVLQ" conf
        poolTMP = getField @"poolTMP" conf
        maxRoundingError = getField @"maxRoundingError" conf

        poolInIx    = getField @"poolInIdx"  redeemer
        poolOutIdx  = getField @"poolOutIdx" redeemer

    selfIn'   <- tlet $ pelemAt # poolInIx # inputs
    selfIn    <- pletFieldsC @'["outRef", "resolved"] selfIn'
    let self = getField @"resolved" selfIn

    PSpending selfRef' <- tmatch (pfromData $ getField @"purpose" ctx)
    let 
        selfIdentity =
            let selfRef   = pfromData $ pfield @"_0" # selfRef'
                selfInRef = pfromData $ getField @"outRef" selfIn
             in selfRef #== selfInRef

    successor <- tlet $ findPoolOutput # poolNft # outputs -- nft is preserved

    succDatum <- tletField @"datum" successor

    selfValue <- tletField @"value" self
    succValue <- tletField @"value" successor

    POutputDatum succD' <- pmatchC succDatum

    succD <- tletField @"outputDatum" succD'

    PDatum succDatum <- pmatchC succD

    succDatum' <- tlet $ ptryFromData @(LMPoolConfig) $ succDatum
    let 
        succPoolDatumEpoch = pfield @"epoch" # succDatum'
        epochAlloc = pdiv # programBudget # epochNum

        reserveAda = assetClassValueOf # selfValue # pAdaAssetClass
        reserveX   = assetClassValueOf # selfValue # poolX
        reserveLQ  = assetClassValueOf # selfValue # poolLQ
        reserveVLQ = assetClassValueOf # selfValue # poolVLQ
        reserveTMP = assetClassValueOf # selfValue # poolTMP

        succAda = assetClassValueOf # succValue # pAdaAssetClass
        succX   = assetClassValueOf # succValue # poolX
        succLQ  = assetClassValueOf # succValue # poolLQ
        succVLQ = assetClassValueOf # succValue # poolVLQ
        succTMP = assetClassValueOf # succValue # poolTMP

        deltaX   = succX - reserveX
        deltaLQ  = succLQ - reserveLQ
        deltaVLQ = succVLQ - reserveVLQ
        deltaTMP = succTMP - reserveTMP

        from = pfield @"from" # validRange
        pextended = pfield @"_0" # from
    
    PFinite req <- pmatchC pextended
    (PPOSIXTime curTime) <- pmatchC $ pfield @"_0" # req
    let 
        curTimeIdx = curTime - programStart + 1
        curEpochIxRem = pmod # curTimeIdx # epochLen
        curEpochIxR = pdiv # curTimeIdx # epochLen

    curEpochIx <-
        tlet $
            pif
                (0 #< curEpochIxRem)
                (curEpochIxR + 1)
                curEpochIxR
    
    validAction <-
        tlet $
            pif
                (deltaLQ #< 0)
                (checkRedeem # deltaVLQ # epochNum # curEpochIx # deltaLQ # deltaTMP)
                (
                    pif
                        (deltaLQ #== 0)
                        (checkCompound # succPoolDatumEpoch # epochNum # curEpochIx # reserveX # epochAlloc # deltaTMP # reserveLQ # reserveAda # succAda # deltaX # deltaLQ # deltaVLQ # execBudget # programBudget)
                        (checkDeposit # deltaLQ # epochNum # curEpochIx # deltaVLQ # deltaTMP # maxRoundingError # programBudget # reserveX # epochAlloc)
                )
    pure $ selfIdentity #&& validAction -- #&& confPreserved

checkDeposit :: 
    Term 
        s 
        ( PInteger 
            :--> PInteger 
            :--> PInteger 
            :--> PInteger 
            :--> PInteger
            :--> PInteger 
            :--> PInteger 
            :--> PInteger 
            :--> PInteger
            :--> PBool
        )
checkDeposit =
    plam $ \releasedVLQ epochNum curEpochIx deltaVLQ deltaTMP maxRoundingError programBudget reservesX epochAlloc ->
        unTermCont $ do
            let
                curEpochMax = pmax # 0 # curEpochIx
                epochsAllocated = epochNum - curEpochMax
                releasedTMP = releasedVLQ * epochsAllocated
                curEpochToCalc = 
                    pif 
                        (curEpochIx #<= epochNum)
                        (curEpochIx)
                        (epochNum + 1)
                prevEpochsCompoundedForDeposit = 
                   (curEpochToCalc - 1) * epochAlloc #<= ((programBudget - reservesX) + maxRoundingError) 
            pure $ prevEpochsCompoundedForDeposit #&& (releasedVLQ #== -deltaVLQ) #&& (releasedTMP #== -deltaTMP)

checkRedeem :: Term s (PInteger :--> PInteger :--> PInteger :--> PInteger :--> PInteger :--> PBool)
checkRedeem =
    plam $ \deltaVLQ epochNum curEpochIx deltaLQ deltaTMP ->
        unTermCont $ do
            let
                curEpochMax = pmax # 0 # curEpochIx
                minReturnedTMP = 
                    pif (epochNum #< curEpochIx)
                epochsDeallocated = epochNum - curEpochMax
                returnedTMP = deltaVLQ * epochsDeallocated
             in pure $ (deltaVLQ #== -deltaLQ) #&& (returnedTMP #== deltaTMP)

checkCompound :: 
    Term 
        s 
        ( PInteger
            :--> PInteger
            :--> PInteger
            :--> PInteger
            :--> PInteger
            :--> PInteger
            :--> PInteger
            :--> PInteger
            :--> PInteger
            :--> PInteger
            :--> PInteger
            :--> PInteger
            :--> PInteger
            :--> PInteger
            :--> PBool
        )
checkCompound =
    plam $ \succPoolDatumEpoch epochNum curEpochIx reservesX epochAlloc deltaTMP reservesLQ execBudgetRem0 execBudgetRem1 deltaX deltaLQ deltaVLQ execBudget programBudget ->
        unTermCont $ do
            let
                epochsToCompound = epochNum - succPoolDatumEpoch
                legalEpoch = succPoolDatumEpoch #<= (curEpochIx - 1)
                prevEpochCompounded = (reservesX - epochsToCompound * epochAlloc) #<= epochAlloc
                reward = pdiv # (epochAlloc * deltaTMP) # reservesLQ -- handle round loss?
                execFee = pdiv # (reward * execBudget) # programBudget
             in pure (legalEpoch 
                  #&& prevEpochCompounded 
                  #&& (-deltaX #== reward) 
                  #&& (deltaLQ #== 0) 
                  #&& (deltaVLQ #== 0) 
                  #&& ((execBudgetRem1 - execBudgetRem0) #<= execFee))

findPoolOutput :: Term s (PAssetClass :--> PBuiltinList PTxOut :--> PTxOut)
findPoolOutput =
    phoistAcyclic $
        plam $ \nft ->
            precList
                ( \self x xs ->
                    let value = pfield @"value" # x
                        amt   = assetClassValueOf # value # nft
                     in pif (amt #== 1) x (self # xs)
                )
                (const $ ptraceError "Pool output not found")