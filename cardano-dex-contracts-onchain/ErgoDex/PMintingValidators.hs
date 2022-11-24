module ErgoDex.PMintingValidators (
    poolNftMiningValidator,
    poolLqMiningValidator,
    lmPoolLqMintValidator,
    wrapMintingValidator,
) where

import Plutarch
import Plutarch.Api.V2 (mkMintingPolicy)
import Plutarch.Api.V2.Contexts (PScriptContext)
import Plutarch.Prelude
import Plutarch.Internal (Config(..), TracingMode(..))
import Plutarch.Unsafe (punsafeCoerce)

import qualified ErgoDex.PContracts.PAssets as A
import PlutusLedgerApi.V1.Scripts (MintingPolicy)
import PlutusLedgerApi.V1.Value (TokenName, AssetClass)
import PlutusLedgerApi.V1.Contexts

cfgForMintingValidator :: Config
cfgForMintingValidator = Config DoTracingAndBinds

wrapMintingValidator ::
    PIsData rdmr =>
    ClosedTerm (rdmr :--> PScriptContext :--> PBool) ->
    ClosedTerm (PData :--> PScriptContext :--> POpaque)
wrapMintingValidator validator = plam $ \rdmr' ctx ->
    let rdmr = pfromData $ punsafeCoerce rdmr'
        result = validator # rdmr # ctx
     in popaque $ pif result (pcon PUnit) (ptraceError "Minting validator reduced to False")

poolNftMiningValidator :: TxOutRef -> TokenName -> MintingPolicy
poolNftMiningValidator oref tn =
    mkMintingPolicy cfgForMintingValidator $
        wrapMintingValidator $
            A.poolNftMintValidatorT (pconstant oref) (pconstant tn)

poolLqMiningValidator :: TxOutRef -> TokenName -> Integer -> MintingPolicy
poolLqMiningValidator oref tn emission =
    mkMintingPolicy cfgForMintingValidator $
        wrapMintingValidator $
            A.poolLqMintValidatorT (pconstant oref) (pconstant tn) (pconstant emission)

lmPoolLqMintValidator :: AssetClass -> Integer -> MintingPolicy
lmPoolLqMintValidator ac emission =
    mkMintingPolicy cfgForMintingValidator $
        wrapMintingValidator $
            A.lmPoolLqMintValidatorT (pconstant ac) (pconstant emission)