module ErgoDex.PContracts.PPoolStakeChangeMintPolicy where

import ErgoDex.PContracts.PApi (ownCurrencySymbol, tletUnwrap, containsSignature, checkPoolNft)
import PExtra.API (assetClass, assetClassValueOf, ptryFromData, PAssetClass(..))
import PExtra.List (pexists)
import PExtra.Monadic
import Plutarch
import Plutarch.Api.V2 
import Plutarch.Api.V1 (PTokenName)
import Plutarch.Api.V1.Value
import Plutarch.Prelude
import Plutarch.Extra.TermCont
import ErgoDex.PContracts.PPool
import ErgoDex.PConstants

extractPoolConfig :: Term s (PTxOut :--> PoolConfig)
extractPoolConfig = plam $ \txOut -> unTermCont $ do
  txOutDatum <- tletField @"datum" txOut

  POutputDatum txOutOutputDatum <- pmatchC txOutDatum

  rawDatum <- tletField @"outputDatum" txOutOutputDatum

  PDatum poolDatum <- pmatchC rawDatum

  tletUnwrap $ ptryFromData @(PoolConfig) $ poolDatum

poolStakeChangeMintPolicyValidatorT :: Term s PAssetClass -> Term s (PBuiltinList PPubKeyHash) -> Term s PInteger -> Term s (PData :--> PScriptContext :--> PBool)
poolStakeChangeMintPolicyValidatorT poolNft adminsPkhs threshold = plam $ \_ ctx -> unTermCont $ do
    txinfo' <- tletField @"txInfo" ctx
    txinfo  <- tcont $ pletFields @'["inputs", "outputs", "signatories"] txinfo'

    inputs  <- tletUnwrap $ getField @"inputs"  txinfo
    outputs <- tletUnwrap $ getField @"outputs" txinfo

    signatories <- tletUnwrap $ getField @"signatories" txinfo

    poolInput' <- tlet $ pelemAt # 0 # inputs
    poolInput  <- pletFieldsC @'["outRef", "resolved"] poolInput'
    let
      poolInputResolved = getField @"resolved" poolInput

    poolInputValue  <- tletField @"value" poolInputResolved

    successor  <- tlet $ findPoolOutput # poolNft # outputs

    poolOutputValue <- tletField @"value" successor

    selfAddr <- tletField @"address" poolInputResolved
    succAddr <- tletField @"address" successor

    succPoolOutputDatum' <- tlet $ extractPoolConfig # successor
    prevCred <- tletField @"credential" selfAddr
    newCred  <- tletField @"credential" succAddr

    newAdminPolicy <- tletField @"stakeAdminPolicy" succPoolOutputDatum'
    let
        correctFinalPolicy = pnull # newAdminPolicy

        validDelta = poolInputValue #== poolOutputValue
        validCred  = prevCred #== newCred

        correctPoolInput = checkPoolNft # poolInputValue # poolNft
                
        validSignaturesQty = 
          pfoldl # plam (\acc pkh -> pif (containsSignature # signatories # pkh) (acc + 1) acc) # 0 # adminsPkhs

        validThreshold = threshold #<= validSignaturesQty

    pure $ validDelta #&& correctFinalPolicy #&& validCred #&& validThreshold #&& correctPoolInput