module ErgoDex.PStakingValidators (
    simpleStakingValidator,
    pkhLockStakingValidator,
    wrapStakingValidator,
) where

import Plutarch
import Plutarch.Api.V2 (mkStakeValidator)
import Plutarch.Api.V2.Contexts (PScriptContext)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import PlutusLedgerApi.V1.Scripts (StakeValidator)
import PlutusLedgerApi.V2         (PubKeyHash)

import qualified ErgoDex.PContracts.PSimpleStaking      as Staking
import qualified ErgoDex.PContracts.PStakingWithPkhLock as PkhStaking

cfgForStakingValidator :: Config
cfgForStakingValidator = Config NoTracing

wrapStakingValidator ::
    PIsData rdmr =>
    ClosedTerm (rdmr :--> PScriptContext :--> PBool) ->
    ClosedTerm (PData :--> PScriptContext :--> POpaque)
wrapStakingValidator validator = plam $ \rdmr' ctx ->
    let rdmr = pfromData $ punsafeCoerce rdmr'
        result = validator # rdmr # ctx
     in popaque $ pif result (pcon PUnit) (ptraceError "Staking validator reduced to False")

simpleStakingValidator :: StakeValidator
simpleStakingValidator = 
   mkStakeValidator cfgForStakingValidator $ wrapStakingValidator Staking.simpleStakingValidatorT

pkhLockStakingValidator :: [PubKeyHash] -> Integer -> StakeValidator
pkhLockStakingValidator authPkhs threshold = 
   mkStakeValidator cfgForStakingValidator $ wrapStakingValidator $ PkhStaking.pkhLockStakingValidatorT (pconstant authPkhs) (pconstant threshold)