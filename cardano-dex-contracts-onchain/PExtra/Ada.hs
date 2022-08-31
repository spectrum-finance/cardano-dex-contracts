module PExtra.Ada (
    pAdaCurrencySymbol,
    pAdaTokenName,
    pAdaAssetClass,
    pIsAda,
) where

import PExtra.API
import Plutarch
import Plutarch.Api.V1
import Plutarch.Prelude

pAdaCurrencySymbol :: Term s PCurrencySymbol
pAdaCurrencySymbol = pcon $ PCurrencySymbol $ phexByteStr ""

pAdaTokenName :: Term s PTokenName
pAdaTokenName = pcon $ PTokenName $ phexByteStr ""

pAdaAssetClass :: Term s PAssetClass
pAdaAssetClass =
    phoistAcyclic $
        pcon $
            PAssetClass $
                pdcons
                    # pdata pAdaCurrencySymbol #$ pdcons
                    # pdata pAdaTokenName #$ pdnil

pIsAda :: Term s (PAssetClass :--> PBool)
pIsAda = plam $ \ac -> ac #== pAdaAssetClass
