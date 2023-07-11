{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.PContracts.PSimpleStaking (
    simpleStakingValidatorT
) where

import Plutarch
import Plutarch.Api.V2.Contexts
import Plutarch.Prelude
import Plutarch.Extra.TermCont

import PExtra.Monadic (tletField)

simpleStakingValidatorT :: Term s (PData :--> PScriptContext :--> PBool)
simpleStakingValidatorT = plam $ \_ ctx' -> unTermCont $ do
    purpose <- tletField @"purpose" ctx'

    pmatchC purpose >>= \case
        PRewarding  _ -> pure . pcon $ PTrue
        PCertifying _ -> pure . pcon $ PTrue
        _             -> pure . pcon $ PFalse