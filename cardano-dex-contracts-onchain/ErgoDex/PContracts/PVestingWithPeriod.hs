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
  let ctx = pfield @"txInfo" # ctx'

  conf     <- pletFieldsC @'["vestingStart", "vestingPeriodDuration", "totalVested", "periodVested", "pkhs", "vestingAC"] conf'
  redeemer <- pletFieldsC @'["vestingPeriodIdx"] redeemer'
  txInfo   <- pletFieldsC @'["validRange", "signatories"] ctx
  let
      vestingStart          = pfromData $ getField @"vestingStart" conf
      vestingPeriodDuration = pfromData $ getField @"vestingPeriodDuration" conf
  
      totalVested  = pfromData $ getField @"totalVested" conf
      periodVested = pfromData $ getField @"periodVested" conf
      pkhs         = pfromData $ getField @"pkhs" conf
      vestingAC    = pfromData $ getField @"vestingAC" conf
      
      vestingPeriodIdx = pfromData $ getField @"vestingPeriodIdx" redeemer
  
      sigs = pfromData $ getField @"signatories" txInfo
  
  validRange <- tletUnwrap $ getField @"validRange" txInfo
  let
      periodAdditionalTime = pmultiply # vestingPeriodIdx # vestingPeriodDuration
  
      periodStartTime = periodAdditionalTime + vestingStart
      validTime       = pbefore # periodStartTime # validRange
      validSignature  = pall # (containsSignature' # sigs) # pkhs

      maxPeriodsQty = pdiv # totalVested # periodVested
      isLastPeriod  = maxPeriodsQty #<= vestingPeriodIdx
  
  correctReward <- 
     tlet $ 
         isLastPeriod #|| (checkRewardAndDatumCorrectness # ctx' # conf' # totalVested # periodVested # vestingPeriodIdx # vestingAC)
  pure $ validTime
       #&& validSignature 
       #&& correctReward

checkRewardAndDatumCorrectness :: Term s (PScriptContext :--> VestingWithPeriodConfig :--> PInteger :--> PInteger :--> PInteger :--> PAssetClass :--> PBool)
checkRewardAndDatumCorrectness = 
    plam $ \ctx prevCfg totalVested periodVested periodId vestingAC -> unTermCont $ do
        let
            selfOutputsList = getContinuingOutputs # ctx
            selfOutput      = phead # selfOutputsList
            selfValue       = pfield @"value" # selfOutput
        txOutDatum <- tletField @"datum" selfOutput

        POutputDatum txOutOutputDatum <- pmatchC txOutDatum

        rawDatum <- tletField @"outputDatum" txOutOutputDatum

        PDatum vestingDatumRaw <- pmatchC rawDatum
        newVestingConfig <- tletUnwrap $ ptryFromData @(VestingWithPeriodConfig) $ vestingDatumRaw
        let    
            correctOutQty   = totalVested - (periodId * periodVested) 
            realOutQty      = assetClassValueOf # selfValue # vestingAC
            correctConfigs  = prevCfg #== newVestingConfig
        pure $ (realOutQty #== correctOutQty) #&& correctConfigs