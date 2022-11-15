module ErgoDex.PValidators (
    poolValidator,
    swapValidator,
    depositValidator,
    redeemValidator,
    vestingValidator,
    validatorAddress,
    vestingWithPeriodValidator,
    wrapValidator,
) where

import PlutusLedgerApi.V1.Scripts (Validator (getValidator))
import PlutusLedgerApi.V1.Address

import qualified ErgoDex.PContracts.PDeposit   as PD
import qualified ErgoDex.PContracts.PPool      as PP
import qualified ErgoDex.PContracts.PRedeem    as PR
import qualified ErgoDex.PContracts.PSwap      as PS
import qualified ErgoDex.PContracts.PVesting   as PV

import qualified ErgoDex.PContracts.PVestingWithPeriod as PVWP

import Plutarch
import Plutarch.Api.V2 (mkValidator, validatorHash)
import Plutarch.Api.V2.Contexts (PScriptContext)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import Plutarch.Internal

cfgForValidator :: Config
cfgForValidator = Config DoTracingAndBinds

wrapValidator ::
    (PIsData dt, PIsData rdmr) =>
    Term s (dt :--> rdmr :--> PScriptContext :--> PBool) ->
    Term s (PData :--> PData :--> PScriptContext :--> POpaque)
wrapValidator validator = plam $ \datum redeemer ctx ->
    let dt = pfromData $ punsafeCoerce datum
        rdmr = pfromData $ punsafeCoerce redeemer
        result = validator # dt # rdmr # ctx
     in popaque $ pif result (pcon PUnit) (ptraceError "Validator reduced to False")

poolValidator :: Validator
poolValidator = mkValidator cfgForValidator $ wrapValidator PP.poolValidatorT

swapValidator :: Validator
swapValidator = mkValidator cfgForValidator $ wrapValidator PS.swapValidatorT

depositValidator :: Validator
depositValidator = mkValidator cfgForValidator $ wrapValidator PD.depositValidatorT

redeemValidator :: Validator
redeemValidator = mkValidator cfgForValidator $ wrapValidator PR.redeemValidatorT

vestingValidator :: Validator
vestingValidator = mkValidator cfgForValidator $ wrapValidator PV.vestingValidatorT

vestingWithPeriodValidator :: Validator
vestingWithPeriodValidator = mkValidator cfgForValidator $ wrapValidator PVWP.vestingWithPeriodValidatorT

validatorAddress :: Validator -> Address
validatorAddress = scriptHashAddress . validatorHash
