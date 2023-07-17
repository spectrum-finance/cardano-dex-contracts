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

poolStakeChangeMintPolicyValidatorT :: Term s PAssetClass -> Term s PPubKeyHash -> Term s (PData :--> PScriptContext :--> PBool)
poolStakeChangeMintPolicyValidatorT poolNft adminPkh = plam $ \_ ctx -> unTermCont $ do
    txinfo' <- tletField @"txInfo" ctx
    txinfo  <- tcont $ pletFields @'["inputs", "outputs", "signatories"] txinfo'

    inputs  <- tletUnwrap $ getField @"inputs"  txinfo
    outputs <- tletUnwrap $ getField @"outputs" txinfo

    signatories <- tletUnwrap $ getField @"signatories" txinfo

    poolInput' <- tlet $ pelemAt # 0 # inputs
    poolInput  <- pletFieldsC @'["outRef", "resolved"] poolInput'
    let
      poolInputResolved = getField @"resolved" poolInput

    feeInput' <- tlet $ pelemAt # 1 # inputs
    feeInput  <- pletFieldsC @'["outRef", "resolved"] feeInput'
    let
      feeInputResolved = getField @"resolved" feeInput
    
    poolInputValue  <- tletField @"value" poolInputResolved
    poolInputConfig <- tlet $ extractPoolConfig # poolInputResolved

    successor  <- tlet $ findPoolOutput # poolNft # outputs

    poolOutputValue <- tletField @"value" successor

    selfAddr <- tletField @"address" poolInputResolved
    succAddr <- tletField @"address" successor

    succPoolOutputDatum' <- tlet $ extractPoolConfig # successor
    prevCred <- tletField @"credential" selfAddr
    newCred  <- tletField @"credential" succAddr

    feeInputDatum <- tletField @"datum" feeInputResolved

    PNoOutputDatum _ <- pmatchC feeInputDatum

    prevConf <- pletFieldsC @'["poolNft", "poolX", "poolY", "poolLq", "feeNum", "lqBound"] poolInputConfig
    newConf  <- pletFieldsC @'["poolNft", "poolX", "poolY", "poolLq", "feeNum", "lqBound"] succPoolOutputDatum'
    let
        prevPoolNft    = getField @"poolNft" prevConf
        prevPoolX      = getField @"poolX"   prevConf
        prevPoolY      = getField @"poolY"   prevConf
        prevPoolLq     = getField @"poolLq"  prevConf
        prevPoolFeeNum = getField @"feeNum"  prevConf

        newPoolNft    = pfromData $ getField @"poolNft" newConf
        newPoolX      = pfromData $ getField @"poolX"   newConf
        newPoolY      = pfromData $ getField @"poolY"   newConf
        newPoolLq     = pfromData $ getField @"poolLq"  newConf
        newPoolFeeNum = pfromData $ getField @"feeNum"  newConf

        validPoolParams = 
            prevPoolNft    #== newPoolNft    #&&
            prevPoolX      #== newPoolX      #&&
            prevPoolY      #== newPoolY      #&&
            prevPoolLq     #== newPoolLq     #&&
            prevPoolFeeNum #== newPoolFeeNum

        strictInputs =
          let inputsLength = plength # inputs
          in inputsLength #== 2

        validDelta = poolInputValue #== poolOutputValue
        validCred  = prevCred #== newCred

        correctPoolInput = checkPoolNft # poolInputValue # poolNft
                
        validSignature = containsSignature # signatories # adminPkh
    
    pure $ strictInputs #&& validDelta #&& validPoolParams #&& validCred #&& validSignature