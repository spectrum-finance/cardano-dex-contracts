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

pkhLockStakingValidatorT :: Term s PPubKeyHash -> Term s (PData :--> PScriptContext :--> PBool)
pkhLockStakingValidatorT authPkh = plam $ \_ ctx' -> unTermCont $ do
    ctx    <- pletFieldsC @'["txInfo", "purpose"] ctx'
    let txInfo' = getField @"txInfo" ctx

    sigs <- tletField @"signatories" txInfo'

    pmatchC (getField @"purpose" ctx) >>= \case
        PRewarding  _ -> pure $ containsSignature # sigs # authPkh
        PCertifying _ -> pure $ containsSignature # sigs # authPkh
        _             -> pure . pcon $ PFalse