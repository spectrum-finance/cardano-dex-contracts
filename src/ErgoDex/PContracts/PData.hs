module ErgoDex.PContracts.PData
  ( pget
  , pget') where

import Plutarch
import Plutarch.Prelude
import Plutarch.Builtin (PBuiltinMap)
import Plutarch.Api.V1 (PMap(..))

pget :: (PIsData a, PIsData b) => Term s (a :--> PMap a b :--> PMaybe b)
pget = plam $ \x xs' -> pmatch xs' $ \case
  PMap xs -> pget' # x # xs

pget' :: (PIsData a, PIsData b) => Term s (a :--> PBuiltinMap a b :--> PMaybe b)
pget' =
  phoistAcyclic $
    plam $ \a ->
      precList
        (\self x xs -> pif (pfstBuiltin # x #== pdata a) (pcon (PJust (pfromData $ psndBuiltin # x))) (self # xs))
        (\_ -> pcon PNothing)
