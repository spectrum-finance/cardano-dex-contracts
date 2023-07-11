module ErgoDex.PContracts.PPoolStakeChangeMintPolicy where

import ErgoDex.PContracts.PApi (ownCurrencySymbol, tletUnwrap, containsSignature)
import PExtra.API (assetClass, assetClassValueOf, ptryFromData)
import PExtra.List (pexists)
import PExtra.Monadic
import Plutarch
import Plutarch.Api.V2
import Plutarch.Api.V1 (PTokenName)
import Plutarch.Api.V1.Value
import Plutarch.Prelude
import Plutarch.Extra.TermCont
import ErgoDex.PContracts.PPool

extractPoolConfig :: Term s (PTxOut :--> PoolConfig)
extractPoolConfig = plam $ \txOut -> unTermCont $ do
  txOutDatum <- tletField @"datum" txOut

  POutputDatum txOutOutputDatum <- pmatchC txOutDatum

  rawDatum <- tletField @"outputDatum" txOutOutputDatum

  PDatum poolDatum <- pmatchC rawDatum

  tletUnwrap $ ptryFromData @(PoolConfig) $ poolDatum

poolStakeChangeMintPolicyValidatorT :: Term s (PData :--> PScriptContext :--> PBool)
poolStakeChangeMintPolicyValidatorT = plam $ \_ ctx -> unTermCont $ do
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
    poolInputConfig <- tlet $ extractPoolConfig # poolInputResolved

    nft <- tletField @"poolNft" poolInputConfig

    successor  <- tlet $ findPoolOutput # nft # outputs

    poolOutputValue <- tletField @"value" successor

    selfAddr <- tletField @"address" poolInputResolved
    succAddr <- tletField @"address" successor

    succPoolOutputDatum' <- tlet $ extractPoolConfig # successor
    prevCred <- tletField @"credential" selfAddr
    newCred  <- tletField @"credential" succAddr
    prevConf <- pletFieldsC @'["poolNft", "poolX", "poolY", "poolLq", "feeNum", "stakeAdmins"] poolInputConfig
    newConf  <- pletFieldsC @'["poolNft", "poolX", "poolY", "poolLq", "feeNum"] succPoolOutputDatum'
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

        validDelta = poolInputValue #== poolOutputValue
        validCred  = prevCred #== newCred
        
        stakeAdmins    = getField @"stakeAdmins" prevConf
        stakeAdmin   = pfromData $ phead # stakeAdmins
        validSignature = containsSignature # signatories # stakeAdmin
    
    pure $ validDelta #&& validPoolParams #&& validCred #&& validSignature