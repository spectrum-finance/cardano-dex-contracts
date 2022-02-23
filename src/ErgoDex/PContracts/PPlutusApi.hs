module ErgoDex.PContracts.PPlutusApi
  ( passetClassValueOf
  , type PAssetClass
  ) where

import Plutarch
import Plutarch.Prelude
import Plutarch.Api.V1.Contexts
import Plutarch.Api.V1 (PCurrencySymbol, PTokenName, PValue(..), PMap)

import ErgoDex.PContracts.PData

type PAssetClass = PTuple PCurrencySymbol PTokenName

passetClassValueOf :: Term s (PValue :--> PAssetClass :--> PInteger)
passetClassValueOf =
  plam $ \vl' cls -> pmatch vl' $ \case
    PValue cs -> pcountAsset # cls # cs 
  where
    pcountAsset :: Term s (PAssetClass :--> PMap PCurrencySymbol (PMap PTokenName PInteger) :--> PInteger)
    pcountAsset = plam $ \cls xs' ->
      let
        csym  = pfield @"_0" # cls
        tname = pfield @"_1" # cls
      in pmatch (pget # csym # xs') $ \case
        PJust ts -> pmatch (pget # tname # ts) $ \case
          PJust v  -> v
          PNothing -> pconstant 0
        PNothing -> pconstant 0
