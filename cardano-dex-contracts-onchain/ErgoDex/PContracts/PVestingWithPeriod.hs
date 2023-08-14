{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.PContracts.PVestingWithPeriod (
    VestingWithPeriodConfig (..),
    vestingWithPeriodValidatorT
) where

import qualified GHC.Generics as GHC

import Plutarch
import Plutarch.List
import Plutarch.Api.V2
import Plutarch.Extra.Interval
import Plutarch.Extra.Api
import Plutarch.DataRepr
import Plutarch.Lift
import Plutarch.Prelude
import Plutarch.Extra.TermCont

import PExtra.API
import PExtra.Monadic (tlet, tletField)
import PExtra.Time

import ErgoDex.PContracts.PApi
import Plutarch.Trace

import qualified ErgoDex.Contracts.Proxy.VestingWithPeriod as VWP

newtype VestingWithPeriodRedeemer (s :: S)
    = VestingWithPeriodRedeemer
        ( Term
            s
            ( PDataRecord
                '[ "vestingInIx"      ':= PInteger
                 , "vestingPeriodIdx" ':= PInteger
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType VestingWithPeriodRedeemer where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl VestingWithPeriodRedeemer where type PLifted VestingWithPeriodRedeemer = VWP.VestingWithPeriodRedeemer
deriving via (DerivePConstantViaData VWP.VestingWithPeriodRedeemer VestingWithPeriodRedeemer) instance (PConstantDecl VWP.VestingWithPeriodRedeemer)

newtype VestingWithPeriodConfig (s :: S)
    = VestingConfig
        ( Term
            s
            ( PDataRecord
                '[ "vestingStart"          ':= PPOSIXTime
                 , "vestingPeriodDuration" ':= PPOSIXTime
                 , "totalVested"           ':= PInteger
                 , "periodVested"          ':= PInteger
                 , "pkhs"                  ':= PBuiltinList (PAsData PPubKeyHash)
                 , "vestingAC"             ':= PAssetClass
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType, PEq)

instance DerivePlutusType VestingWithPeriodConfig where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl VestingWithPeriodConfig where type PLifted VestingWithPeriodConfig = VWP.VestingWithPeriodConfig
deriving via (DerivePConstantViaData VWP.VestingWithPeriodConfig VestingWithPeriodConfig) instance (PConstantDecl VWP.VestingWithPeriodConfig)

instance PTryFrom PData (PAsData VestingWithPeriodConfig)

vestingWithPeriodValidatorT :: ClosedTerm (VestingWithPeriodConfig :--> VestingWithPeriodRedeemer :--> PScriptContext :--> PBool)
vestingWithPeriodValidatorT = plam $ \conf' redeemer' ctx' -> unTermCont $ do
  ctx <- pletFieldsC @'["txInfo", "purpose"] ctx'
  let txinfo' = getField @"txInfo" ctx

  conf     <- pletFieldsC @'["vestingStart", "vestingPeriodDuration", "totalVested", "periodVested", "pkhs", "vestingAC"] conf'
  redeemer <- pletFieldsC @'["vestingPeriodIdx", "vestingInIx"] redeemer'
  txInfo   <- pletFieldsC @'["validRange", "signatories", "inputs"] txinfo'
  inputs   <- tletUnwrap $ getField @"inputs" txInfo
  let
      vestingStart          = pfromData $ getField @"vestingStart" conf
      vestingPeriodDuration = pfromData $ getField @"vestingPeriodDuration" conf
  
      totalVested  = pfromData $ getField @"totalVested" conf
      periodVested = pfromData $ getField @"periodVested" conf
      pkhs         = pfromData $ getField @"pkhs" conf
      vestingAC    = pfromData $ getField @"vestingAC" conf
      
      vestingPeriodIdx = pfromData $ getField @"vestingPeriodIdx" redeemer
      vestingInIdx     = pfromData $ getField @"vestingInIx" redeemer
  
      sigs = pfromData $ getField @"signatories" txInfo
  
  validRange <- tletUnwrap $ getField @"validRange" txInfo

  selfIn' <- tlet $ pelemAt # vestingInIdx # inputs
  selfIn  <- pletFieldsC @'["outRef", "resolved"] selfIn'
  let
      periodAdditionalTime = pmultiply # vestingPeriodIdx # vestingPeriodDuration
  
      periodStartTime = periodAdditionalTime + vestingStart
      validTime       = pbefore # periodStartTime # validRange
      validSignature  = pall # (containsSignature' # sigs) # pkhs

      maxPeriodsQty = pdiv # totalVested # periodVested
      isLastPeriod  = maxPeriodsQty #<= vestingPeriodIdx

      self = getField @"resolved" selfIn

  selfAddr <- tletField @"address" self
  
  PSpending selfRef' <- pmatchC $ getField @"purpose" ctx

  selfRef <- tletField @"_0" selfRef'
  let 
    selfInRef    = getField @"outRef" selfIn
    selfIdentity = selfRef #== selfInRef -- self is the output currently validated by this script

  correctReward <- 
     tlet $ 
         isLastPeriod #|| (checkRewardAndDatumCorrectness # ctx' # conf' # totalVested # periodVested # vestingPeriodIdx # vestingAC # selfAddr)
  pure $ validTime
       #&& validSignature 
       #&& correctReward
       #&& selfIdentity

checkRewardAndDatumCorrectness :: Term s (PScriptContext :--> VestingWithPeriodConfig :--> PInteger :--> PInteger :--> PInteger :--> PAssetClass :--> PAddress :--> PBool)
checkRewardAndDatumCorrectness = 
    plam $ \ctx prevCfg totalVested periodVested periodId vestingAC selfAddr -> unTermCont $ do
        let
            selfOutputsList = getContinuingOutputs # ctx
            selfOutput      = phead # selfOutputsList
            selfValue       = pfield @"value" # selfOutput
        txOutDatum <- tletField @"datum" selfOutput
        succAddr   <- tletField @"address" selfOutput

        POutputDatum txOutOutputDatum <- pmatchC txOutDatum

        rawDatum <- tletField @"outputDatum" txOutOutputDatum

        PDatum vestingDatumRaw <- pmatchC rawDatum
        newVestingConfig <- tletUnwrap $ ptryFromData @(VestingWithPeriodConfig) $ vestingDatumRaw
        let    
            correctOutQty   = totalVested - (periodId * periodVested) 
            realOutQty      = assetClassValueOf # selfValue # vestingAC
            correctConfigs  = prevCfg #== newVestingConfig
            correctAddress  = succAddr #== selfAddr
        pure $ (realOutQty #== correctOutQty) #&& correctConfigs #&& correctAddress