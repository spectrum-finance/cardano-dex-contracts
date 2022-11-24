module ErgoDex.PValidators (
    poolValidator,
    swapValidator,
    depositValidator,
    redeemValidator,
    lmRedeemValidator,
    lmStakingBundleValidator,
    validatorAddress,
    wrapValidator,
) where

import PlutusLedgerApi.V1.Scripts (Validator (getValidator))
import PlutusLedgerApi.V1.Address

import qualified ErgoDex.PContracts.LqMining.Simple.Redeem        as LMR
import qualified ErgoDex.PContracts.LqMining.Simple.StakingBundle as LMSB

import qualified ErgoDex.PContracts.PDeposit as PD
import qualified ErgoDex.PContracts.PPool as PP
import qualified ErgoDex.PContracts.PRedeem as PR
import qualified ErgoDex.PContracts.PSwap as PS

import Plutarch
import Plutarch.Api.V2 (mkValidator, validatorHash)
import Plutarch.Api.V2.Contexts (PScriptContext)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

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
poolValidator = mkValidator $ wrapValidator PP.poolValidatorT

swapValidator :: Validator
swapValidator = mkValidator $ wrapValidator PS.swapValidatorT

depositValidator :: Validator
depositValidator = mkValidator $ wrapValidator PD.depositValidatorT

redeemValidator :: Validator
redeemValidator = mkValidator $ wrapValidator PR.redeemValidatorT

lmRedeemValidator :: Validator
lmRedeemValidator = mkValidator $ wrapValidator LMR.redeemValidatorT

lmStakingBundleValidator :: Validator
lmStakingBundleValidator = mkValidator $ wrapValidator LMSB.stakingBundleValidatorT

validatorAddress :: Validator -> Address
validatorAddress = scriptHashAddress . validatorHash
