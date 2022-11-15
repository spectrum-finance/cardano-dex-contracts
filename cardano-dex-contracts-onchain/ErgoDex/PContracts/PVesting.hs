{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.PContracts.PVesting (
    VestingConfig (..),
    vestingValidatorT
) where

import qualified GHC.Generics as GHC

import Plutarch
import Plutarch.Api.V2
import Plutarch.Extra.Interval
import Plutarch.Api.V1.Time
import Plutarch.DataRepr
import Plutarch.Lift
import Plutarch.Prelude
import Plutarch.Extra.TermCont
import Plutarch.Api.V1.Interval (PInterval)
import qualified Plutarch.Monadic as P

import PExtra.API
import PExtra.Monadic (tlet, tletField)

import ErgoDex.PContracts.PApi

import qualified ErgoDex.Contracts.Proxy.Vesting as V

newtype VestingRedeemer (s :: S)
    = VestingRedeemer
        ( Term
            s
            ( PDataRecord
                '[ "vestingInIx" ':= PInteger
                 , "rewardOutIx" ':= PInteger
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType VestingRedeemer where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl VestingRedeemer where type PLifted VestingRedeemer = V.VestingRedeemer
deriving via (DerivePConstantViaData V.VestingRedeemer VestingRedeemer) instance (PConstantDecl V.VestingRedeemer)

newtype VestingConfig (s :: S)
    = VestingConfig
        ( Term
            s
            ( PDataRecord
                '[ "deadline"  ':= PPOSIXTime
                 , "pkh"       ':= PPubKeyHash
                 , "vestingAC" ':= PAssetClass
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType VestingConfig where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl VestingConfig where type PLifted VestingConfig = V.VestingConfig
deriving via (DerivePConstantViaData V.VestingConfig VestingConfig) instance (PConstantDecl V.VestingConfig)

vestingValidatorT :: ClosedTerm (VestingConfig :--> VestingRedeemer :--> PScriptContext :--> PBool)
vestingValidatorT = plam $ \conf' redeemer' ctx' -> unTermCont $ do
  ctx  <- pletFieldsC @'["txInfo", "purpose"] ctx'
  conf <- pletFieldsC @'["deadline", "pkh", "vestingAC"] conf'
  let
    deadline  = pfromData $ getField @"deadline" conf
    pkh       = pfromData $ getField @"pkh" conf
    vestingAC = pfromData $ getField @"vestingAC" conf

  txInfo   <- pletFieldsC @'["inputs", "outputs", "validRange", "signatories"] $ getField @"txInfo" ctx
  redeemer <- pletFieldsC @'["vestingInIx", "rewardOutIx"] redeemer'
  let
    vestingInIx  = getField @"vestingInIx" redeemer
    rewardOutIx  = getField @"rewardOutIx" redeemer

  validRange <- tletUnwrap $ getField @"validRange" txInfo
  inputs     <- tletUnwrap $ getField @"inputs" txInfo
  outputs    <- tletUnwrap $ getField @"outputs" txInfo

  selfIn'   <- tlet $ pelemAt # vestingInIx # inputs
  selfIn    <- pletFieldsC @'["outRef", "resolved"] selfIn'
  selfValue <-
      let self = getField @"resolved" selfIn
       in tletField @"value" self

  PSpending selfRef' <- pmatchC $ getField @"purpose" ctx

  selfRef <- tletField @"_0" selfRef'
  let
    selfInRef    = getField @"outRef" selfIn
    selfIdentity = selfRef #== selfInRef

    sigs = pfromData $ getField @"signatories" txInfo
    validSignature = containsSignature # sigs # pkh

    validTime = pbefore # deadline # validRange

  rewardOut   <- tlet $ pelemAt # rewardOutIx # outputs
  rewardValue <- tlet $ getRewardValueByPKH' # rewardOut # pkh
  let
    vestingIn     = assetClassValueOf # selfValue   # vestingAC
    vestingOut    = assetClassValueOf # rewardValue # vestingAC
    correctReward = vestingIn #== vestingOut

  pure $ validSignature #&& correctReward #&& validTime #&& selfIdentity