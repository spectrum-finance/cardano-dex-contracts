{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.PContracts.PDeposit (
    DepositConfig (..),
    depositValidatorT,
) where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic, I (I))

import Plutarch
import Plutarch.Api.V2.Contexts
import Plutarch.DataRepr
import Plutarch.Lift
import Plutarch.Prelude
import Plutarch.Api.V1 (PMaybeData, PPubKeyHash, PValue)
import Plutarch.Extra.TermCont

import PExtra.API
import PExtra.Ada
import PExtra.List (pelemAt)
import PExtra.Monadic (tlet, tletField, tmatch)

import ErgoDex.PContracts.PApi (containsSignature, getRewardValue', maxLqCap, pmin, tletUnwrap)
import ErgoDex.PContracts.POrder (OrderAction (Apply, Refund), OrderRedeemer)

import qualified ErgoDex.Contracts.Proxy.Deposit as D

newtype DepositConfig (s :: S)
    = DepositConfig
        ( Term
            s
            ( PDataRecord
                '[ "poolNft" ':= PAssetClass
                 , "x" ':= PAssetClass
                 , "y" ':= PAssetClass
                 , "lq" ':= PAssetClass
                 , "exFee" ':= PInteger
                 , "rewardPkh" ':= PPubKeyHash
                 , "stakePkh" ':= PMaybeData PPubKeyHash
                 , "collateralAda" ':= PInteger
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType DepositConfig where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl DepositConfig where type PLifted DepositConfig = D.DepositConfig
deriving via (DerivePConstantViaData D.DepositConfig DepositConfig) instance (PConstantDecl D.DepositConfig)

depositValidatorT :: ClosedTerm (DepositConfig :--> OrderRedeemer :--> PScriptContext :--> PBool)
depositValidatorT = plam $ \conf' redeemer' ctx' -> unTermCont $ do
    ctx  <- pletFieldsC @'["txInfo", "purpose"] ctx'
    conf <- pletFieldsC @'["x", "y", "lq", "poolNft", "exFee", "rewardPkh", "stakePkh", "collateralAda"] conf'
    let 
        collateralAda = getField @"collateralAda" conf

        rewardPkh = getField @"rewardPkh" conf
        stakePkh  = getField @"stakePkh"  conf
        exFee     = getField @"exFee" conf

        x  = getField @"x"  conf
        y  = getField @"y"  conf
        lq = getField @"lq" conf

        txInfo' = getField @"txInfo" ctx
    
    txInfo  <- pletFieldsC @'["inputs", "outputs", "signatories"] txInfo'
    inputs  <- tletUnwrap $ getField @"inputs"  txInfo
    outputs <- tletUnwrap $ getField @"outputs" txInfo

    redeemer <- pletFieldsC @'["poolInIx", "orderInIx", "rewardOutIx", "action"] redeemer'
    let 
      poolInIx    = getField @"poolInIx"    redeemer
      orderInIx   = getField @"orderInIx"   redeemer
      rewardOutIx = getField @"rewardOutIx" redeemer
      action = getField @"action" redeemer

    rewardOut   <- tlet $ pelemAt # rewardOutIx # outputs
    rewardValue <- tlet $ getRewardValue' # rewardOut # rewardPkh # stakePkh

    poolIn'   <- tlet $ pelemAt # poolInIx # inputs
    poolIn    <- pletFieldsC @'["outRef", "resolved"] poolIn'
    poolValue <-
        let pool = pfromData $ getField @"resolved" poolIn
         in tletField @"value" pool
    let poolIdentity =
            let requiredNft = pfromData $ getField @"poolNft" conf
                nftAmount = assetClassValueOf # poolValue # requiredNft
             in nftAmount #== 1

    selfIn'   <- tlet $ pelemAt # orderInIx # inputs
    selfIn    <- pletFieldsC @'["outRef", "resolved"] selfIn'
    selfValue <-
        let self = pfromData $ getField @"resolved" selfIn
         in tletField @"value" self

    PSpending selfRef' <- pmatchC $ getField @"purpose" ctx
    let 
        selfIdentity =
            let selfRef   = pfield @"_0" # selfRef'
                selfInRef = getField @"outRef" selfIn
             in selfRef #== selfInRef

        strictInputs =
            let inputsLength = plength # inputs
             in inputsLength #== 2

    liquidity <-
        let lqNegative = assetClassValueOf # poolValue # lq
         in tlet $ maxLqCap - lqNegative

    reservesX <- tlet $ assetClassValueOf # poolValue # x
    reservesY <- tlet $ assetClassValueOf # poolValue # y

    minRewardByX <- tlet $ minAssetReward # selfValue # x # reservesX # liquidity # exFee # collateralAda
    minRewardByY <- tlet $ minAssetReward # selfValue # y # reservesY # liquidity # exFee # collateralAda
    let validChange =
            pif
                (minRewardByX #== minRewardByY)
                (pcon PTrue)
                ( pif
                    (minRewardByX #< minRewardByY)
                    (validChange' # rewardValue # y # minRewardByY # minRewardByX # reservesY # liquidity)
                    (validChange' # rewardValue # x # minRewardByX # minRewardByY # reservesX # liquidity)
                )
        minReward = pmin # minRewardByX # minRewardByY
        validReward =
            let actualReward = assetClassValueOf # rewardValue # lq
             in minReward #<= actualReward

    pure $
        pmatch action $ \case
            Apply -> poolIdentity #&& selfIdentity #&& strictInputs #&& validChange #&& validReward
            Refund ->
                let sigs = pfromData $ getField @"signatories" txInfo
                 in containsSignature # sigs # rewardPkh

-- Checks whether an asset overflow is returned back to user
validChange' :: Term s (PValue _ _ :--> PAssetClass :--> PInteger :--> PInteger :--> PInteger :--> PInteger :--> PBool)
validChange' =
    phoistAcyclic $
        plam $ \rewardValue overflowAsset overflowAssetInput otherAssetInput overflowAssetReserves liquidity ->
            let diff = overflowAssetInput - otherAssetInput
                excess = pdiv # (diff * overflowAssetReserves) # liquidity
                change = assetClassValueOf # rewardValue # overflowAsset
             in excess #<= change

minAssetReward :: Term s (PValue _ _ :--> PAssetClass :--> PInteger :--> PInteger :--> PInteger :--> PInteger :--> PInteger)
minAssetReward =
    phoistAcyclic $
        plam $ \selfValue asset assetReserves liquidity exFee collateralAda ->
            unTermCont $ do
                assetInput <- tlet $ assetClassValueOf # selfValue # asset
                let depositInput = pif (pIsAda # asset) (assetInput - exFee - collateralAda) assetInput
                pure $ pdiv # (depositInput * liquidity) # assetReserves
