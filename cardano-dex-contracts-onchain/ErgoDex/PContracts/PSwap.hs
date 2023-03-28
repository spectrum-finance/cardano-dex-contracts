{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.PContracts.PSwap (
    SwapConfig (..),
    swapValidatorT,
) where

import qualified GHC.Generics as GHC

import Plutarch
import Plutarch.Api.V2
import Plutarch.Api.V1.Tuple
import Plutarch.Api.V1.Value
import Plutarch.Api.V2.Contexts
import Plutarch.DataRepr
import Plutarch.Lift
import Plutarch.Prelude
import Plutarch.Extra.TermCont

import PExtra.API
import PExtra.Ada (pIsAda)
import PExtra.Monadic (tlet, tletField, tmatch)

import ErgoDex.PContracts.PApi
import ErgoDex.PContracts.POrder (OrderAction (..), OrderRedeemer (..))

import qualified ErgoDex.Contracts.Proxy.Swap as S

newtype SwapConfig (s :: S)
    = SwapConfig
        ( Term
            s
            ( PDataRecord
                '[ "base" ':= PAssetClass
                 , "quote" ':= PAssetClass
                 , "poolNft" ':= PAssetClass
                 , "feeNum" ':= PInteger -- numerator of pool fee
                 , "exFeePerTokenNum" ':= PInteger -- numerator of execution fee
                 , "exFeePerTokenDen" ':= PInteger -- denominator of execution fee
                 , "rewardPkh" ':= PPubKeyHash -- PublicKeyHash of user
                 , "stakePkh" ':= PMaybeData PPubKeyHash
                 , "baseAmount" ':= PInteger
                 , "minQuoteAmount" ':= PInteger -- minimal quote output that satisfies user
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType SwapConfig where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl SwapConfig where type PLifted SwapConfig = S.SwapConfig
deriving via (DerivePConstantViaData S.SwapConfig SwapConfig) instance (PConstantDecl S.SwapConfig)

swapValidatorT :: ClosedTerm (SwapConfig :--> OrderRedeemer :--> PScriptContext :--> PBool)
swapValidatorT = plam $ \conf' redeemer' ctx' -> unTermCont $ do
    ctx    <- pletFieldsC @'["txInfo", "purpose"] ctx'
    conf   <- pletFieldsC @'["base", "quote", "poolNft", "feeNum", "exFeePerTokenNum", "exFeePerTokenDen", "rewardPkh", "stakePkh", "baseAmount", "minQuoteAmount"] conf'
    let
        rewardPkh   = getField @"rewardPkh" conf
        stakePkh    = getField @"stakePkh"  conf
        requiredNft = getField @"poolNft"   conf

        base   = getField @"base"   conf
        quote  = getField @"quote"  conf
        feeNum = getField @"feeNum" conf

        exFeePerTokenNum = getField @"exFeePerTokenNum" conf
        exFeePerTokenDen = getField @"exFeePerTokenDen" conf

        baseAmount = getField @"baseAmount"     conf
        minOutput  = getField @"minQuoteAmount" conf
    
    txInfo  <- pletFieldsC @'["inputs", "outputs", "signatories"] $ getField @"txInfo" ctx
    inputs  <- tletUnwrap $ getField @"inputs"  txInfo
    outputs <- tletUnwrap $ getField @"outputs" txInfo

    redeemer <- pletFieldsC @'["poolInIx", "orderInIx", "rewardOutIx", "action"] redeemer'
    let
        poolInIx    = getField @"poolInIx"    redeemer
        orderInIx   = getField @"orderInIx"   redeemer
        rewardOutIx = getField @"rewardOutIx" redeemer
        action      = getField @"action"      redeemer

    rewardOut   <- tlet $ pelemAt # rewardOutIx # outputs
    rewardValue <- tlet $ getRewardValue' # rewardOut # rewardPkh # stakePkh

    poolIn'   <- tlet $ pelemAt # poolInIx # inputs
    poolIn    <- pletFieldsC @'["outRef", "resolved"] poolIn'
    poolValue <- 
        let pool = getField @"resolved" poolIn
         in tletField @"value" pool
    let poolIdentity = -- operation is performed with the pool selected by the user 
            let nftAmount = assetClassValueOf # poolValue # requiredNft
             in nftAmount #== 1

    selfIn'   <- tlet $ pelemAt # orderInIx # inputs
    selfIn    <- pletFieldsC @'["outRef", "resolved"] selfIn'
    selfValue <-
        let self = getField @"resolved" selfIn
         in tletField @"value" self

    PSpending selfRef' <- tmatch (pfromData $ getField @"purpose" ctx)
    let 
        selfIdentity =
            let selfRef   = pfromData $ pfield @"_0" # selfRef'
                selfInRef = pfromData $ getField @"outRef" selfIn
             in selfRef #== selfInRef -- check that orderInIx points to the actual order

        quoteIn  = assetClassValueOf # selfValue   # quote
        quoteOut = assetClassValueOf # rewardValue # quote

    quoteDelta <- tlet $ quoteOut - quoteIn
    quoteAmount <-
        tlet $
            pif
                (pIsAda # quote)
                (pdiv # (quoteDelta * exFeePerTokenDen) # (exFeePerTokenDen - exFeePerTokenNum))
                quoteDelta
    fairExFee <-
        tlet $ 
            (pIsAda # quote) #|| (validExFee # rewardValue # selfValue # base # baseAmount # quoteAmount # exFeePerTokenNum # exFeePerTokenDen)
    let 
        strictInputs = -- ensure double satisfaction attack is not possible
            let inputsLength = plength # inputs
             in inputsLength #== 2
        minSatisfaction = minOutput #<= quoteAmount -- configured minimal output is satisfied
        fairPrice = validPrice # quoteAmount # poolValue # base # quote # baseAmount # feeNum

    pure $
        pmatch action $ \case
            Apply -> poolIdentity #&& selfIdentity #&& strictInputs #&& minSatisfaction #&& fairExFee #&& fairPrice
            Refund ->
                let sigs = pfromData $ getField @"signatories" txInfo
                 in containsSignature # sigs # rewardPkh -- user signed the refund

 -- ADA excess is returned to user
validExFee ::
    Term
        s
        ( PValue _ _ 
            :--> PValue _ _
            :--> PAssetClass
            :--> PInteger
            :--> PInteger
            :--> PInteger
            :--> PInteger
            :--> PBool
        )
validExFee =
    plam $ \rewardValue selfValue base baseAmount quoteAmount exFeePerTokenNum exFeePerTokenDen ->
        unTermCont $ do
            zeroAsData' <- tlet zeroAsData
            bqAda <-
                tlet $
                    pif
                        (pIsAda # base)
                        (pdata baseAmount)
                        (zeroAsData')
            let baseAda  = pfromData $ bqAda
                outAda   = plovelaceValueOf # rewardValue
                inAda    = plovelaceValueOf # selfValue
                exFee    = pdiv # (quoteAmount * exFeePerTokenNum) # exFeePerTokenDen
            pure $ (inAda - baseAda - exFee) #<= outAda

 -- Swap price is adequate to pool reserves according to constant product formula.
validPrice ::
    Term
        s
        ( PInteger
            :--> PValue _ _
            :--> PAssetClass
            :--> PAssetClass
            :--> PInteger
            :--> PInteger
            :--> PBool
        )
validPrice =
    plam $ \quoteAmount poolValue base quote baseAmount feeNum ->
        let relaxedOut    = quoteAmount + 1
            reservesBase  = assetClassValueOf # poolValue # base
            reservesQuote = assetClassValueOf # poolValue # quote
            correctOut    = pdiv # (reservesQuote * baseAmount * feeNum) # (reservesBase * feeDen + baseAmount * feeNum)
         in correctOut #<= relaxedOut
