module ErgoDex.PContracts.PStaking where

import Plutarch
import Plutarch.Prelude
import Plutarch.Api.V1
import PExtra.Monadic
import PExtra.API (PAssetClass)

poolAdaStakingValidatorT :: Term s PAssetClass -> Term s (PData :--> PScriptContext :--> PBool) -- poolInIx -> _ -> Bool
poolAdaStakingValidatorT poolNft = plam $ \_ ctx -> unTermCont $ do
  txinfo' <- tletField @"txInfo" ctx
  txinfo  <- tcont $ pletFields @'["inputs", "outputs", "wdrl"] txinfo'
  -- poolOut <- findPoolOutput poolNft outputs
  -- poolIn  <- tlet $ pelemAt # poolInIx # inputs
  -- cond1: poolOut.adaValue - poolIn.adaValue >= wdrl.value (withdrawal to pool)
  -- cond2: poolOut.tokenValue - poolIn.tokenValue == 0      (token reserves unchanged)
  undefined
