module ErgoDex.PContracts.PAssets (
    poolNftMintValidatorT,
    poolLqMintValidatorT,
) where

import ErgoDex.PContracts.PApi (ownCurrencySymbol, tletUnwrap)
import PExtra.API (assetClass, assetClassValueOf)
import PExtra.List (pexists)
import PExtra.Monadic
import Plutarch
import Plutarch.Api.V2
import Plutarch.Api.V1 (PTokenName)
import Plutarch.Prelude

poolNftMintValidatorT :: Term s PTxOutRef -> Term s PTokenName -> Term s (PData :--> PScriptContext :--> PBool)
poolNftMintValidatorT oref tn = plam $ \_ ctx -> unTermCont $ do
    txinfo' <- tletField @"txInfo" ctx
    txinfo <- tcont $ pletFields @'["inputs", "mint"] txinfo'
    inputs <- tletUnwrap $ getField @"inputs" txinfo
    let targetUtxoConsumed =
            let isTarget i = unTermCont $ do
                    let oref' = pfield @"outRef" # i
                    pure $ oref' #== oref
             in pexists # plam (isTarget) # inputs
        tokenMintExact = unTermCont $ do
            valueMint <- tlet $ getField @"mint" txinfo
            let ownAc = assetClass # (ownCurrencySymbol # ctx) # tn
            pure $ assetClassValueOf # valueMint # ownAc #== 1
    pure $ targetUtxoConsumed #&& tokenMintExact

poolLqMintValidatorT :: Term s PTxOutRef -> Term s PTokenName -> Term s PInteger -> Term s (PData :--> PScriptContext :--> PBool)
poolLqMintValidatorT oref tn emission = plam $ \_ ctx -> unTermCont $ do
    txinfo' <- tletField @"txInfo" ctx
    txinfo <- tcont $ pletFields @'["inputs", "mint"] txinfo'
    inputs <- tletUnwrap $ getField @"inputs" txinfo
    let targetUtxoConsumed =
            let isTarget i = unTermCont $ do
                    oref' <- tletField @"outRef" i
                    pure $ oref' #== oref
             in pexists # plam (isTarget) # inputs
        tokenMintExact = unTermCont $ do
            valueMint <- tlet $ getField @"mint" txinfo
            let ownAc = assetClass # (ownCurrencySymbol # ctx) # tn
            pure $ assetClassValueOf # valueMint # ownAc #== emission
    pure $ targetUtxoConsumed #&& tokenMintExact
