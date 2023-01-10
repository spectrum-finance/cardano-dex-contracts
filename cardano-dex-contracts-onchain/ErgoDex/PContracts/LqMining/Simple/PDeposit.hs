{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.PContracts.LqMining.Simple.PDeposit where

import qualified GHC.Generics  as GHC

import Plutarch
import Plutarch.Api.V2              (POutputDatum(POutputDatum), PPubKeyHash, PDatum(PDatum), PTokenName(..), PCurrencySymbol(..))
import Plutarch.Api.V2.Contexts     (PScriptContext, PScriptPurpose (PSpending))
import Plutarch.DataRepr
import Plutarch.Lift
import Plutarch.Prelude
import Plutarch.Extra.TermCont

import PExtra.API                   (PAssetClass, assetClassValueOf, ptryFromData, assetClass, tletUnwrap)
import PExtra.Monadic               (tlet, tletField, tmatch)

import ErgoDex.PContracts.PApi      (containsSignature, getRewardValueByPkh', maxLqCap)

import qualified ErgoDex.Contracts.Proxy.LqMining.Simple.Deposit   as D
import qualified ErgoDex.PContracts.LqMining.Simple.PStakingBundle as SB
import qualified ErgoDex.PContracts.LqMining.Simple.PLMPool        as LMPool

newtype DepositConfig (s :: S)
    = DepositConfig
        ( Term
            s
            ( PDataRecord
                '[ "expectedNumEpochs" ':= PInteger
                 , "bundleKeyCS" ':= PCurrencySymbol
                 , "redeemerPkh" ':= PPubKeyHash
                 , "vlqAC" ':= PAssetClass
                 , "tmpAC" ':= PAssetClass
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType DepositConfig where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl DepositConfig where type PLifted DepositConfig = D.DepositConfig
deriving via (DerivePConstantViaData D.DepositConfig DepositConfig) instance (PConstantDecl D.DepositConfig)

newtype DepositRedeemer (s :: S)
    = DepositRedeemer
        ( Term
            s
            ( PDataRecord
                '[ "poolInIdx"      ':= PInteger
                 , "depositInIdx"   ':= PInteger
                 , "redeemerOutIdx" ':= PInteger
                 , "bundleOutIdx"   ':= PInteger
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType DepositRedeemer where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl DepositRedeemer where type PLifted DepositRedeemer = D.DepositRedeemer
deriving via (DerivePConstantViaData D.DepositRedeemer DepositRedeemer) instance (PConstantDecl D.DepositRedeemer)

depositValidatorT :: ClosedTerm (DepositConfig :--> DepositRedeemer :--> PScriptContext :--> PBool)
depositValidatorT = plam $ \conf' redeemer' ctx' -> unTermCont $ do
    ctx      <- pletFieldsC @'["txInfo", "purpose"] ctx'
    conf     <- pletFieldsC @'["expectedNumEpochs", "bundleKeyCS", "redeemerPkh", "vlqAC", "tmpAC"] conf'
    redeemer <- pletFieldsC @'["poolInIdx", "depositInIdx", "redeemerOutIdx", "bundleOutIdx"] redeemer'

    txInfo  <- pletFieldsC @'["inputs", "outputs", "signatories"] $ getField @"txInfo" ctx
    inputs  <- tletUnwrap $ getField @"inputs"  txInfo
    outputs <- tletUnwrap $ getField @"outputs" txInfo
    let
        poolInIx       = getField @"poolInIdx"      redeemer
        depositInIdx   = getField @"depositInIdx"   redeemer
        redeemerOutIdx = getField @"redeemerOutIdx" redeemer
        bundleOutIdx   = getField @"bundleOutIdx"   redeemer

        expectedNumEpochs = getField @"expectedNumEpochs" conf

        redeemerPkh   = getField @"redeemerPkh" conf
        vlqAC         = getField @"vlqAC"       conf
        bundleKeyCS   = getField @"bundleKeyCS" conf
        tmpAC         = getField @"tmpAC"       conf

        sigs = pfromData $ getField @"signatories" txInfo

        signedByRedeemPkh = containsSignature # sigs # redeemerPkh
    
    selfIn'   <- tlet $ pelemAt # depositInIdx # inputs
    selfIn    <- pletFieldsC @'["outRef", "resolved"] selfIn'
    selfValue <-
        let self = getField @"resolved" selfIn
         in tletField @"value" self

    PSpending selfRef' <- tmatch (pfromData $ getField @"purpose" ctx)

    let 
        selfIdentity =
            let selfRef   = pfromData $ pfield @"_0" # selfRef'
                selfInRef = pfromData $ getField @"outRef" selfIn
             in selfRef #== selfInRef

    poolIn'    <- tlet $ pelemAt # poolInIx # inputs
    poolOutRef <- tletUnwrap $ pfield @"outRef" # poolIn'
    let
        poolId     = pfield @"id" # poolOutRef
        lqTnBytes  = pcon $ PTokenName $ pfield @"_0" # poolId

    redeemerOut'  <- tlet $ pelemAt # redeemerOutIdx # outputs
    let 
        redeemerValue = getRewardValueByPkh' # redeemerOut' # redeemerPkh
        
        lqAc = assetClass # bundleKeyCS # lqTnBytes

        redeemerLqValue = assetClassValueOf # redeemerValue # lqAc
        correctLqValue  = redeemerLqValue #== maxLqCap
    
    bundleOut'  <- tlet $ pelemAt # bundleOutIdx # outputs
    bundleOut   <- pletFieldsC @'["value", "datum"] bundleOut'
    let 
        bundleValue = getField @"value" bundleOut
        datumOD'    = getField @"datum" bundleOut

    POutputDatum bundleOD' <- pmatchC datumOD'
    
    bundleOD <- tletField @"outputDatum" bundleOD'

    PDatum bundleDatum'' <- pmatchC bundleOD

    bundleDatum' <- tlet $ ptryFromData @(SB.StakingBundleConfig) $ bundleDatum''
    bundleDatum  <- pletFieldsC @'["bundleLQAC", "redeemerPkh"] bundleDatum'
    let
        bundleLQAC = getField @"bundleLQAC" bundleDatum

        redeemerPkhBundle = getField @"redeemerPkh" bundleDatum

        vlqIn  = assetClassValueOf # selfValue   # vlqAC
        vlqOut = assetClassValueOf # bundleValue # vlqAC
        tmpOut = assetClassValueOf # bundleValue # tmpAC

        validVLQQty = vlqIn #== vlqOut
        validTMPQty = (vlqIn * expectedNumEpochs) #== tmpOut

        validBundleRedeemer = redeemerPkh #== redeemerPkhBundle

        validBundleAC = bundleLQAC #== lqAc
    
    pure $ signedByRedeemPkh #|| (selfIdentity #&& validVLQQty #&& validTMPQty #&& validBundleRedeemer #&& correctLqValue #&& validBundleAC)