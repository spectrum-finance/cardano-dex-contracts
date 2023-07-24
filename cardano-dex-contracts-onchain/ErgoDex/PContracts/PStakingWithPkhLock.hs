{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.PContracts.PStakingWithPkhLock (
    pkhLockStakingValidatorT
) where

import Plutarch
import Plutarch.Api.V2.Contexts
import Plutarch.Prelude
import Plutarch.Extra.TermCont
import Plutarch.Api.V2          (PPubKeyHash)

import ErgoDex.PContracts.PApi  (containsSignature)
import PExtra.Monadic           (tletField)

pkhLockStakingValidatorT :: Term s (PBuiltinList PPubKeyHash) -> Term s PInteger -> Term s (PData :--> PScriptContext :--> PBool)
pkhLockStakingValidatorT adminsPkhs threshold = plam $ \_ ctx' -> unTermCont $ do
    ctx    <- pletFieldsC @'["txInfo", "purpose"] ctx'
    let txInfo' = getField @"txInfo" ctx

    sigs <- tletField @"signatories" txInfo'

    let 
        validSignaturesQty = pfoldl # plam (\acc pkh -> pif (containsSignature # sigs # pkh) (acc + 1) acc) # 0 # adminsPkhs
        validThreshold = threshold #<= validSignaturesQty

    pmatchC (getField @"purpose" ctx) >>= \case
        PRewarding  _ -> pure validThreshold
        PCertifying _ -> pure validThreshold
        _             -> pure . pcon $ PFalse