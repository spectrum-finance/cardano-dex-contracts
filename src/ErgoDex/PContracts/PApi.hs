module ErgoDex.PContracts.PApi
 ( hasValidSignatories
 , containsSignature
 , getRewardValue
 , getRewardValue'
 , tletUnwrap
 , pmin
 , getInputValue
 , checkPoolNft
 , checkInputsQty
 , zero
 , zeroAsData
 , getPoolDatum
 , maxLqCap
 , feeDen
 ) where

import Plutarch
import Plutarch.Api.V1
import Plutarch.Prelude

import PExtra.List
import PExtra.Monadic (tlet, tletField)
import PExtra.API

maxLqCap :: Term s PInteger
maxLqCap = pconstant 0x7fffffffffffffff

feeDen :: Term s PInteger
feeDen = pconstant 1000

zero :: Term s PInteger
zero = pconstant 0

zeroAsData :: Term s (PAsData PInteger)
zeroAsData = pdata zero

tletUnwrap :: (PIsData a) => Term s (PAsData a) -> TermCont @r s (Term s a)
tletUnwrap = tlet . pfromData

pmin :: POrd a => Term s (a :--> a :--> a)
pmin = phoistAcyclic $ plam $ \a b -> pif (a #<= b) a b

hasValidSignatories :: Term s (PTxInfo :--> PPubKeyHash :--> PBool)
hasValidSignatories = phoistAcyclic $ plam $ \txInfo userPubKeyHash -> unTermCont $ do
  let signatories = pfield @"signatories" # txInfo
  pure (pelem # pdata userPubKeyHash # signatories)

containsSignature :: Term s (PBuiltinList (PAsData PPubKeyHash) :--> PPubKeyHash :--> PBool)
containsSignature = phoistAcyclic $ plam $ \signatories userPubKeyHash -> pelem # pdata userPubKeyHash # signatories

getRewardValue :: Term s (PTxInfo :--> PInteger :--> PPubKeyHash :--> PValue)
getRewardValue = phoistAcyclic $ plam $ \txInfo idx pubkeyHash -> unTermCont $ do
  outputs      <- tletUnwrap $ pfield @"outputs" # txInfo
  let output   = pelemAt # idx # outputs
  adr          <- tletUnwrap $ pfield @"address" # output
  pure $
      pmatch (pfromData $ pfield @"credential" # adr) $ \case
        PPubKeyCredential cred -> unTermCont $ do
          pkhOut <- tletUnwrap $ pfield @"_0" # cred
          vl     <- tletUnwrap $ pfield @"value" # output
          pure $ pif (pkhOut #== pubkeyHash)
            vl
            (ptraceError "Invalid reward proposition")
        _ -> ptraceError "Invalid reward proposition"

-- Guarantees reward prposition correctness
getRewardValue' :: Term s (PAsData PTxOut :--> PPubKeyHash :--> PValue)
getRewardValue' = phoistAcyclic $ plam $ \out pubkeyHash -> unTermCont $ do
  let addr = pfield @"address" # out
  cred <- tletField @"credential" addr
  pure $ pmatch cred $ \case
    PPubKeyCredential pcred ->
      let
        pkh   = pfield @"_0" # pcred
        value = pfield @"value" # out
      in pif (pkh #== pubkeyHash) value (ptraceError "Invalid reward proposition")
    _ -> ptraceError "Invalid reward proposition"

getInputValue :: Term s (PBuiltinList (PAsData PTxInInfo) :--> PInteger :--> PValue)
getInputValue = phoistAcyclic $ plam $ \inputs index -> unTermCont $ do
  possiblePoolInput <- tletUnwrap (pelemAt # index # inputs)
  output            <- tletUnwrap $ pfield @"resolved" # possiblePoolInput
  pure $ pfield @"value" # output

checkPoolNft :: Term s (PValue :--> PAssetClass :--> PBool)
checkPoolNft = phoistAcyclic $ plam $ \value poolNft ->
  let nftQty = assetClassValueOf # value # poolNft
  in pif (nftQty #== 1)
    (pcon PTrue)
    (ptraceError "Invalid pool")

checkInputsQty :: Term s (PBuiltinList (PAsData PTxInInfo) :--> PBool)
checkInputsQty = phoistAcyclic $ plam $ \inputs ->
  let inputsLength = plength # inputs
  in inputsLength #== 2

getPoolDatum :: Term s (PInteger :--> PTxInfo :--> PDatum)
getPoolDatum = phoistAcyclic $ plam $ \index txInfo -> unTermCont $ do
  datums     <- tletUnwrap $ pfield @"data" # txInfo
  maybeTuple <- tlet $ pfromData (pelemAt # index # datums)
  tletUnwrap $ pfield @"_1" # maybeTuple
