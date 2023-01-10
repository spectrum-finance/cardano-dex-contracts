{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.PContracts.LqMining.Simple.PRedeem where

import qualified GHC.Generics  as GHC

import Plutarch
import Plutarch.Api.V2           (PPubKeyHash)
import Plutarch.Api.V2.Contexts  (PScriptContext)
import Plutarch.DataRepr
import Plutarch.Lift
import Plutarch.Prelude
import Plutarch.Extra.TermCont

import PExtra.API     (PAssetClass, assetClassValueOf)
import PExtra.Monadic (tlet, tletField)

import           ErgoDex.PContracts.PApi (getRewardValueByPkh')
import qualified ErgoDex.Contracts.Proxy.LqMining.Simple.Redeem as R

newtype RedeemConfig (s :: S)
    = RedeemConfig
        ( Term
            s
            ( PDataRecord
                '[ "expectedLQAC"     ':= PAssetClass
                 , "expectedLQAmount" ':= PInteger
                 , "redeemerPkh"      ':= PPubKeyHash
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType RedeemConfig where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl RedeemConfig where type PLifted RedeemConfig = R.RedeemConfig
deriving via (DerivePConstantViaData R.RedeemConfig RedeemConfig) instance (PConstantDecl R.RedeemConfig)

newtype RedeemRedeemerConfig (s :: S)
    = RedeemRedeemerConfig
        ( Term
            s
            ( PDataRecord
                '[ "rewardOutIdx" ':= PInteger ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType RedeemRedeemerConfig where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl RedeemRedeemerConfig where type PLifted RedeemRedeemerConfig = R.RedeemRedeemerConfig
deriving via (DerivePConstantViaData R.RedeemRedeemerConfig RedeemRedeemerConfig) instance (PConstantDecl R.RedeemRedeemerConfig)

redeemValidatorT :: ClosedTerm (RedeemConfig :--> RedeemRedeemerConfig :--> PScriptContext :--> PBool)
redeemValidatorT = plam $ \conf' redeemer' ctx' -> unTermCont $ do
    txInfo   <- tletField @"txInfo" ctx'
    outputs  <- tletField @"outputs" $ txInfo
    conf     <- pletFieldsC @'["expectedLQAC", "expectedLQAmount", "redeemerPkh"] conf'

    rewardOutIdx <- tletField @"rewardOutIdx" redeemer'
    let
        expectedLQAC     = getField @"expectedLQAC"     conf
        expectedLQAmount = getField @"expectedLQAmount" conf
        redeemerPkh      = getField @"redeemerPkh"      conf

    rewardOut' <- tlet $ pelemAt # rewardOutIdx # outputs
    let 
        rewardValue = getRewardValueByPkh' # rewardOut' # redeemerPkh

        tmpOut = assetClassValueOf # rewardValue # expectedLQAC

        correctReward = tmpOut #== expectedLQAmount
        
    pure $ correctReward