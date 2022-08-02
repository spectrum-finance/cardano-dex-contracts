{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.PContracts.PSwap (
    SwapConfig (..),
    swapValidatorT,
) where

import qualified GHC.Generics as GHC
import Plutarch
import Plutarch.Api.V1
import Plutarch.Api.V1.Contexts
import Plutarch.DataRepr
import Plutarch.Lift
import Plutarch.Prelude

import Generics.SOP (Generic, I (I))
import PExtra.API
import PExtra.Ada (pGetLovelace, pIsAda)
import PExtra.List (pelemAt)
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
                 , "feeNum" ':= PInteger
                 , "exFeePerTokenNum" ':= PInteger
                 , "exFeePerTokenDen" ':= PInteger
                 , "rewardPkh" ':= PPubKeyHash
                 , "stakePkh" ':= PMaybeData PPubKeyHash
                 , "baseAmount" ':= PInteger
                 , "minQuoteAmount" ':= PInteger
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving anyclass (Generic, PIsDataRepr)
    deriving
        (PMatch, PIsData, PDataFields, PlutusType)
        via (PIsDataReprInstances SwapConfig)

instance PUnsafeLiftDecl SwapConfig where type PLifted SwapConfig = S.SwapConfig
deriving via (DerivePConstantViaData S.SwapConfig SwapConfig) instance (PConstant S.SwapConfig)

swapValidatorT :: ClosedTerm (SwapConfig :--> OrderRedeemer :--> PScriptContext :--> PBool)
swapValidatorT = plam $ \conf' redeemer' ctx' -> unTermCont $ do
    ctx <- tcont $ pletFields @'["txInfo", "purpose"] ctx'
    conf <- tcont $ pletFields @'["base", "quote", "poolNft", "feeNum", "exFeePerTokenNum", "exFeePerTokenDen", "rewardPkh", "stakePkh", "baseAmount", "minQuoteAmount"] conf'
    txInfo' <- tletUnwrap $ hrecField @"txInfo" ctx
    txInfo <- tcont $ pletFields @'["inputs", "outputs", "signatories"] txInfo'
    inputs <- tletUnwrap $ hrecField @"inputs" txInfo
    outputs <- tletUnwrap $ hrecField @"outputs" txInfo

    redeemer <- tcont $ pletFields @'["poolInIx", "orderInIx", "rewardOutIx", "action"] redeemer'
    poolInIx <- tletUnwrap $ hrecField @"poolInIx" redeemer
    orderInIx <- tletUnwrap $ hrecField @"orderInIx" redeemer
    rewardOutIx <- tletUnwrap $ hrecField @"rewardOutIx" redeemer

    rewardOut <- tlet $ pelemAt # rewardOutIx # outputs
    rewardPkh <- tletUnwrap $ hrecField @"rewardPkh" conf
    stakePkh <- tletUnwrap $ hrecField @"stakePkh" conf
    rewardValue <- tlet $ getRewardValue' # rewardOut # rewardPkh # stakePkh

    poolIn' <- tlet $ pelemAt # poolInIx # inputs
    poolIn <- tcont $ pletFields @'["outRef", "resolved"] poolIn'
    poolValue <-
        let pool = pfromData $ hrecField @"resolved" poolIn
         in tletField @"value" pool
    let poolIdentity =
            let requiredNft = pfromData $ hrecField @"poolNft" conf
                nftAmount = assetClassValueOf # poolValue # requiredNft
             in nftAmount #== 1

    selfIn' <- tlet $ pelemAt # orderInIx # inputs
    selfIn <- tcont $ pletFields @'["outRef", "resolved"] selfIn'
    selfValue <-
        let self = pfromData $ hrecField @"resolved" selfIn
         in tletField @"value" self

    PSpending selfRef' <- tmatch (pfromData $ hrecField @"purpose" ctx)
    let selfIdentity =
            let selfRef = pfromData $ pfield @"_0" # selfRef'
                selfInRef = pfromData $ hrecField @"outRef" selfIn
             in selfRef #== selfInRef

    base <- tletUnwrap $ hrecField @"base" conf
    quote <- tletUnwrap $ hrecField @"quote" conf
    exFeePerTokenNum <- tletUnwrap $ hrecField @"exFeePerTokenNum" conf
    exFeePerTokenDen <- tletUnwrap $ hrecField @"exFeePerTokenDen" conf
    baseAmount <- tletUnwrap $ hrecField @"baseAmount" conf
    let quoteIn = assetClassValueOf # selfValue # quote
        quoteOut = assetClassValueOf # rewardValue # quote
    quoteDelta <- tlet $ quoteOut - quoteIn
    quoteAmount <-
        tlet $
            pif
                (pIsAda # quote)
                (pdiv # (quoteDelta * exFeePerTokenDen) # (exFeePerTokenDen - exFeePerTokenNum))
                quoteDelta

    let strictInputs =
            let inputsLength = plength # inputs
             in inputsLength #== 2 -- address double satisfaction attack
        minSatisfaction =
            let minOutput = pfromData $ hrecField @"minQuoteAmount" conf
             in minOutput #<= quoteAmount
        fairExFee = validExFee # rewardValue # selfValue # base # baseAmount # quote # quoteAmount # exFeePerTokenNum # exFeePerTokenDen
        fairPrice =
            let feeNum = pfromData $ hrecField @"feeNum" conf
             in validPrice # quoteAmount # poolValue # base # quote # baseAmount # feeNum

    action <- tletUnwrap $ hrecField @"action" redeemer
    pure $
        pmatch action $ \case
            Apply -> poolIdentity #&& selfIdentity #&& strictInputs #&& minSatisfaction #&& fairExFee #&& fairPrice
            Refund ->
                let sigs = pfromData $ hrecField @"signatories" txInfo
                 in containsSignature # sigs # rewardPkh

validExFee ::
    Term
        s
        ( PValue
            :--> PValue
            :--> PAssetClass
            :--> PInteger
            :--> PAssetClass
            :--> PInteger
            :--> PInteger
            :--> PInteger
            :--> PBool
        )
validExFee =
    plam $ \rewardValue selfValue base baseAmount quote quoteAmount exFeePerTokenNum exFeePerTokenDen ->
        unTermCont $ do
            zeroAsData' <- tlet zeroAsData
            bqAda <-
                tlet $
                    pif
                        (pIsAda # base)
                        (ptuple # pdata baseAmount # zeroAsData')
                        (pif (pIsAda # quote) (ptuple # zeroAsData' # pdata quoteAmount) (ptuple # zeroAsData' # zeroAsData'))
            let baseAda = pfromData $ pfield @"_0" # bqAda
                quoteAda = pfromData $ pfield @"_1" # bqAda
                outAda = pGetLovelace # rewardValue
                inAda = pGetLovelace # selfValue
                exFee = pdiv # (quoteAmount * exFeePerTokenNum) # exFeePerTokenDen
            pure $ (inAda - baseAda - exFee) #<= (outAda - quoteAda)

validPrice ::
    Term
        s
        ( PInteger
            :--> PValue
            :--> PAssetClass
            :--> PAssetClass
            :--> PInteger
            :--> PInteger
            :--> PBool
        )
validPrice =
    plam $ \quoteAmount poolValue base quote baseAmount feeNum ->
        let relaxedOut = quoteAmount + 1
            reservesBase = assetClassValueOf # poolValue # base
            reservesQuote = assetClassValueOf # poolValue # quote
         in reservesQuote * baseAmount * feeNum #<= relaxedOut * (reservesBase * feeDen + baseAmount * feeNum)
