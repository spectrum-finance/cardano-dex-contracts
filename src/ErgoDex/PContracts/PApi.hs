module ErgoDex.PContracts.PApi
 ( hasValidSignatories
 , getRewardValue
 , tletUnwrap
 , getInputValue
 , poolCheckNft
 , validInputsQty
 , zeroInteger
 ) where

import Plutarch
import Plutarch.Api.V1
import Plutarch.Prelude

import PExtra.List
import PExtra.Monadic (tlet)
import PExtra.API

tletUnwrap :: (PIsData a) => Term s (PAsData a) -> TermCont @r s (Term s a)
tletUnwrap = tlet . pfromData

hasValidSignatories :: Term s (PTxInfo :--> PPubKeyHash :--> PBool)
hasValidSignatories = plam $ \txInfo userPubKeyHash -> unTermCont $ do
  let signatories = pfield @"signatories" # txInfo
  pure (pelem # pdata userPubKeyHash # signatories)

getRewardValue :: Term s (PTxInfo :--> PInteger :--> PPubKeyHash :--> PValue)
getRewardValue = plam $ \txInfo idx pubkeyHash -> unTermCont $ do
  outputs      <- tletUnwrap $ pfield @"outputs" # txInfo
  let output   = (pelemAt # idx # outputs)
  adr          <- tletUnwrap $ pfield @"address" # output
  pure $
      pmatch (pfromData $ pfield @"credential" # adr) $ (\case
        PPubKeyCredential cred -> unTermCont $ do
          pkhOut <- tletUnwrap $ pfield @"_0" # cred
          vl     <- tletUnwrap $ pfield @"value" # output
          pure $ pif (pkhOut #== pubkeyHash)
            vl
            (ptraceError "Invalid reward proposition")
        _ -> ptraceError "Invalid reward proposition")

getInputValue :: Term s (PBuiltinList (PAsData PTxInInfo) :--> PInteger :--> PValue)
getInputValue = plam $ \inputs index -> unTermCont $ do
  possiblePoolInput <- tletUnwrap (pelemAt # index # inputs)
  output            <- tletUnwrap $ pfield @"resolved" # possiblePoolInput
  pure $ pfield @"value" # output

poolCheckNft :: Term s (PValue :--> PAssetClass :--> PUnit)
poolCheckNft = plam $ \value poolNft -> unTermCont $ do
  let nftQty =  assetClassValueOf # value # poolNft
  pure $ pif (nftQty #== 1)
    (pcon PUnit)
    (ptraceError "Invalid pool")

validInputsQty :: Term s (PBuiltinList (PAsData PTxInInfo) :--> PBool)
validInputsQty = plam $ \inputs -> unTermCont $ do
  let inputsLength = plength # inputs
  pure $ inputsLength #== 2

zeroInteger :: Term s (PAsData PInteger)
zeroInteger = pdata 0