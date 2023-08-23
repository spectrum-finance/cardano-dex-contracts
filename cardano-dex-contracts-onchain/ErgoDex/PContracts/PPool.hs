{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.PContracts.PPool (
    PoolConfig (..),
    PoolAction (..),
    PoolRedeemer (..),
    readPoolState,
    findPoolOutput,
    poolValidatorT,
) where

import qualified GHC.Generics as GHC
import           Generics.SOP (Generic, I (I))

import Plutarch
import Plutarch.Api.V2              (PMaybeData (..), PTxOut, POutputDatum(..), PAddress(..), PPubKeyHash(..), PDatum(..), PValue(..), KeyGuarantees(..), AmountGuarantees(..), PCurrencySymbol(..))
import Plutarch.Api.V2.Contexts     (PScriptContext, PScriptPurpose (PSpending), PTxInfo(..))
import Plutarch.DataRepr
import Plutarch.Lift
import Plutarch.Prelude
import Plutarch.Extra.TermCont
import Plutarch.Builtin             (pasInt, pforgetData, pfromData, pdata, PIsData(..))
import Plutarch.Unsafe              (punsafeCoerce)
import Plutarch.Internal.PlutusType (PInner, PlutusType, pcon', pmatch')

import PExtra.API                   (PAssetClass, assetClassValueOf, ptryFromData, assetClass, pValueLength)
import PExtra.List                  (pelemAt)
import PExtra.Monadic               (tcon, tlet, tletField, tmatch)

import qualified ErgoDex.Contracts.Pool     as P
import           ErgoDex.PContracts.PApi    (burnLqInitial, feeDen, maxLqCap, tletUnwrap, zero, containsSignature)
import           ErgoDex.PConstants

newtype PoolConfig (s :: S)
    = PoolConfig
        ( Term
            s
            ( PDataRecord
                '[ "poolNft"          ':= PAssetClass
                 , "poolX"            ':= PAssetClass
                 , "poolY"            ':= PAssetClass
                 , "poolLq"           ':= PAssetClass
                 , "feeNum"           ':= PInteger
                 , "stakeAdminPolicy" ':= PBuiltinList (PAsData PCurrencySymbol)
                 , "lqBound"          ':= PInteger
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType PoolConfig where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PoolConfig where type PLifted PoolConfig = P.PoolConfig
deriving via (DerivePConstantViaData P.PoolConfig PoolConfig) instance (PConstantDecl P.PoolConfig)

instance PTryFrom PData (PAsData PoolConfig)

data PoolAction (s :: S) = Deposit | Redeem | Swap | Destroy | ChangeStakingPool

instance PIsData PoolAction where
    pfromDataImpl tx =
        let x = pasInt # pforgetData tx
         in pmatch' x pcon
    pdataImpl x = pmatch x (punsafeCoerce . pdata . pcon')

instance PlutusType PoolAction where
    type PInner PoolAction = PInteger

    pcon' Deposit = 0
    pcon' Redeem = 1
    pcon' Swap = 2
    pcon' Destroy = 3
    pcon' ChangeStakingPool = 4

    pmatch' x f =
        pif
            (x #== 0)
            (f Deposit)
            ( pif
                (x #== 1)
                (f Redeem)
                ( pif
                    (x #== 2)
                    (f Swap)
                    (pif (x #== 3) (f Destroy) (f ChangeStakingPool))
                )
            )

newtype PoolRedeemer (s :: S)
    = PoolRedeemer
        ( Term
            s
            ( PDataRecord
                '[ "action" ':= PoolAction
                 , "selfIx" ':= PInteger
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType PoolRedeemer where type DPTStrat _ = PlutusTypeData

newtype PoolState (s :: S)
    = PoolState
        ( Term
            s
            ( PDataRecord
                '[ "reservesX"   ':= PInteger
                 , "reservesY"   ':= PInteger
                 , "liquidity"   ':= PInteger
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType PoolState where type DPTStrat _ = PlutusTypeData

newtype PoolDiff (s :: S)
    = PoolDiff
        ( Term
            s
            ( PDataRecord
                '[ "diffX" ':= PInteger
                 , "diffY" ':= PInteger
                 , "diffLq" ':= PInteger
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType PoolDiff where type DPTStrat _ = PlutusTypeData

readPoolState :: Term s (PoolConfig :--> PTxOut :--> PoolState)
readPoolState = phoistAcyclic $
    plam $ \conf' out -> unTermCont $ do
        conf  <- pletFieldsC @'["poolX", "poolY", "poolLq"] conf'
        let
            poolX  = getField @"poolX"  conf
            poolY  = getField @"poolY"  conf
            poolLq = getField @"poolLq" conf

        value <- tletField @"value" out
        
        let 
            x = pdata $ assetClassValueOf # value # poolX
            y = pdata $ assetClassValueOf # value # poolY
            negLq = assetClassValueOf # value # poolLq
            lq = pdata $ maxLqCap - negLq
        tcon $
            PoolState $
                pdcons @"reservesX" @PInteger # x
                    #$ pdcons @"reservesY" @PInteger # y
                    #$ pdcons @"liquidity" @PInteger # lq
                        # pdnil

-- Guarantees preservation of pool NFT
findPoolOutput :: Term s (PAssetClass :--> PBuiltinList PTxOut :--> PTxOut)
findPoolOutput =
    phoistAcyclic $
        plam $ \nft ->
            precList
                ( \self x xs ->
                    let value = pfield @"value" # x
                        amt   = assetClassValueOf # value # nft
                     in pif (amt #== 1) x (self # xs)
                )
                (const $ ptraceError "Pool output not found")

poolCheckStakeChange :: ClosedTerm (PoolConfig :--> PTxInfo :--> PBool)
poolCheckStakeChange = plam $ \cfg txInfo -> unTermCont $ do
  valueMint <- tletField @"mint" txInfo
  policies  <- tletField @"stakeAdminPolicy" cfg
  let 
      policyCS = pfromData $ phead # policies
      mintedAc = assetClass # policyCS # poolStakeChangeMintTokenNameP
  pure $ assetClassValueOf # valueMint # mintedAc #== 1

poolValidatorT :: ClosedTerm (PoolConfig :--> PoolRedeemer :--> PScriptContext :--> PBool)
poolValidatorT = plam $ \conf redeemer' ctx' -> unTermCont $ do
    redeemer <- pletFieldsC @'["action", "selfIx"] redeemer'
    let
        selfIx = getField @"selfIx" redeemer
        action = getField @"action" redeemer

    ctx <- pletFieldsC @'["txInfo", "purpose"] ctx'
    let txinfo' = getField @"txInfo" ctx

    txInfo  <- pletFieldsC @'["inputs", "outputs"] txinfo'
    inputs  <- tletUnwrap $ getField @"inputs" txInfo
    selfIn' <- tlet $ pelemAt # selfIx # inputs
    selfIn  <- pletFieldsC @'["outRef", "resolved"] selfIn'

    PSpending selfRef' <- pmatchC $ getField @"purpose" ctx

    selfRef <- tletField @"_0" selfRef'
    let 
        selfInRef    = getField @"outRef" selfIn
        selfIdentity = selfRef #== selfInRef -- self is the output currently validated by this script

        selfInput = getField @"resolved" selfIn
        
    s0  <- tlet $ readPoolState # conf # selfInput
    lq0 <- tletField @"liquidity" s0

    pure $
        selfIdentity #&& (pmatch action $ \case
            Destroy -> lq0 #<= burnLqInitial -- all tokens except for permanetly locked ones are removed
            _ -> unTermCont $ do
                outputs <- tletUnwrap $ getField @"outputs" txInfo

                nft <- tletField @"poolNft" conf

                successorOut <- tlet $ findPoolOutput # nft # outputs -- nft is preserved
                successor <- pletFieldsC @'["datum", "address", "value"] successorOut
                self      <- pletFieldsC @'["datum", "address", "value"] selfInput

                s1  <- tlet $ readPoolState # conf # successorOut
                rx0 <- tletField @"reservesX" s0
                rx1 <- tletField @"reservesX" s1
                ry0 <- tletField @"reservesY" s0
                ry1 <- tletField @"reservesY" s1
                lq1 <- tletField @"liquidity" s1
                let dx  = rx1 - rx0
                    dy  = ry1 - ry0
                    dlq = lq1 - lq0 -- pool keeps only the negative part of LQ tokens

                selfDatum <- tletUnwrap $ getField @"datum" self
                succDatum <- tletUnwrap $ getField @"datum" successor

                POutputDatum selfD' <- pmatchC selfDatum
                POutputDatum succD' <- pmatchC succDatum

                lqBound <- tletField @"lqBound" conf

                selfD <- tletField @"outputDatum" selfD'
                succD <- tletField @"outputDatum" succD'
                let 
                    confPreserved = selfD #== succD -- config preserved
                    swapAllowed   = lqBound #<= (rx0 * 2)

                selfValue     <- tletUnwrap $ getField @"value" self
                succesorValue <- tletUnwrap $ getField @"value" successor

                let 
                    selfValueLength = pValueLength # selfValue
                    succesorValueLength = pValueLength # succesorValue

                    noMoreTokens = selfValueLength #== succesorValueLength

                selfAddr <- tletUnwrap $ getField @"address" self
                succAddr <- tletUnwrap $ getField @"address" successor
                let 
                    scriptPreserved = succAddr #== selfAddr -- validator, staking cred preserved
                    valid = pmatch action $ \case
                        Swap -> unTermCont $ do
                            feeNum  <- tletField @"feeNum" conf
                            feeDen' <- tlet feeDen
                            let
                                dxf = dx * feeNum
                                dyf = dy * feeNum
                                validSwap =
                                    pif
                                        (zero #< dx)
                                        (-dy * (rx0 * feeDen' + dxf) #<= ry0 * dxf)
                                        (-dx * (ry0 * feeDen' + dyf) #<= rx0 * dyf)                            
                            pure $ noMoreTokens #&& swapAllowed #&& confPreserved #&& scriptPreserved #&& dlq #== 0 #&& validSwap -- liquidity left intact and swap is performed properly
                        ChangeStakingPool -> poolCheckStakeChange # conf # txinfo'
                        _ -> 
                            let
                              validDepositRedeem = dlq * rx0 #<= dx * lq0 #&& dlq * ry0 #<= dy * lq0
                            in noMoreTokens #&& confPreserved #&& scriptPreserved #&& validDepositRedeem -- either deposit or redeem is performed properly                
                pure valid
            )
