{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.PContracts.PPool (
    PoolConfig (..),
    PoolAction (..),
    PoolRedeemer (..),
    poolValidatorT,
) where

import qualified GHC.Generics as GHC
import           Generics.SOP (Generic, I (I))

import Plutarch
import Plutarch.Api.V2              (PMaybeData (PDJust), PTxOut, POutputDatum(POutputDatum, PNoOutputDatum, POutputDatumHash))
import Plutarch.Api.V2.Contexts     (PScriptContext, PScriptPurpose (PSpending))
import Plutarch.DataRepr
import Plutarch.Lift
import Plutarch.Prelude
import Plutarch.Extra.TermCont
import Plutarch.Builtin             (pasInt, pforgetData, pfromData, pdata, PIsData(..))
import Plutarch.Unsafe              (punsafeCoerce)
import Plutarch.Internal.PlutusType (PInner, PlutusType, pcon', pmatch')

import PExtra.API                   (PAssetClass, assetClassValueOf)
import PExtra.List                  (pelemAt)
import PExtra.Monadic               (tcon, tlet, tletField, tmatch)

import qualified ErgoDex.Contracts.Pool  as P
import           ErgoDex.PContracts.PApi (burnLqInitial, feeDen, maxLqCap, tletUnwrap, zero)

newtype PoolConfig (s :: S)
    = PoolConfig
        ( Term
            s
            ( PDataRecord
                '[ "poolNft" ':= PAssetClass
                 , "poolX"   ':= PAssetClass
                 , "poolY"   ':= PAssetClass
                 , "poolLq"  ':= PAssetClass
                 , "feeNum"  ':= PInteger
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType PoolConfig where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PoolConfig where type PLifted PoolConfig = P.PoolConfig
deriving via (DerivePConstantViaData P.PoolConfig PoolConfig) instance (PConstantDecl P.PoolConfig)

data PoolAction (s :: S) = Deposit | Redeem | Swap | Destroy

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

    pmatch' x f =
        pif
            (x #== 0)
            (f Deposit)
            ( pif
                (x #== 1)
                (f Redeem)
                (pif (x #== 2) (f Swap) (f Destroy))
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
                '[ "reservesX" ':= PInteger
                 , "reservesY" ':= PInteger
                 , "liquidity" ':= PInteger
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

validDepositRedeem :: Term s (PoolState :--> PInteger :--> PInteger :--> PInteger :--> PBool)
validDepositRedeem = phoistAcyclic $
    plam $ \state' dx dy dlq -> unTermCont $ do
        state <- pletFieldsC @'["reservesX", "reservesY", "liquidity"] state'
        let
            rx = getField @"reservesX" state
            ry = getField @"reservesY" state
            lq = getField @"liquidity" state

        pure $ dlq * rx #<= dx * lq #&& dlq * ry #<= dy * lq

validSwap :: Term s (PoolConfig :--> PoolState :--> PInteger :--> PInteger :--> PBool)
validSwap = phoistAcyclic $
    plam $ \conf state' dx dy -> unTermCont $ do
        state <- pletFieldsC @'["reservesX", "reservesY"] state'
        let 
            rx = getField @"reservesX" state
            ry = getField @"reservesY" state

        feeNum  <- tletField @"feeNum" conf
        feeDen' <- tlet feeDen

        let
           dxf = dx * feeNum
           dyf = dy * feeNum
        pure $
            pif
                (zero #< dx)
                (-dy * (rx * feeDen' + dxf) #<= ry * dxf)
                (-dx * (ry * feeDen' + dyf) #<= rx * dyf)

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
    let self = getField @"resolved" selfIn

    s0  <- tlet $ readPoolState # conf # self
    lq0 <- tletField @"liquidity" s0

    pure $
        pmatch action $ \case
            Destroy -> lq0 #<= burnLqInitial -- all tokens except for permanetly locked ones are removed
            _ -> unTermCont $ do
                outputs <- tletUnwrap $ getField @"outputs" txInfo
                nft     <- tletField @"poolNft" conf

                successor <- tlet $ findPoolOutput # nft # outputs -- nft is preserved

                s1  <- tlet $ readPoolState # conf # successor
                rx0 <- tletField @"reservesX" s0
                rx1 <- tletField @"reservesX" s1
                ry0 <- tletField @"reservesY" s0
                ry1 <- tletField @"reservesY" s1
                lq1 <- tletField @"liquidity" s1
                let dx  = rx1 - rx0
                    dy  = ry1 - ry0
                    dlq = lq1 - lq0 -- pool keeps only the negative part of LQ tokens

                PSpending selfRef' <- pmatchC $ getField @"purpose" ctx

                selfRef <- tletField @"_0" selfRef'
                let 
                    selfInRef    = getField @"outRef" selfIn
                    selfIdentity = selfRef #== selfInRef -- self is the output currently validated by this script

                selfDatum <- tletField @"datum" self
                succDatum <- tletField @"datum" successor

                POutputDatum selfD' <- pmatchC selfDatum
                POutputDatum succD' <- pmatchC succDatum

                selfD <- tletField @"outputDatum" selfD'
                succD <- tletField @"outputDatum" succD'
                let 
                    confPreserved = selfD #== succD -- config preserved

                selfAddr <- tletField @"address" self
                succAddr <- tletField @"address" successor
                let 
                    scriptPreserved = succAddr #== selfAddr -- validator, staking cred preserved
                    validAction     = pmatch action $ \case
                        Swap -> dlq #== 0 #&& validSwap # conf # s0 # dx # dy
                        _ -> validDepositRedeem # s0 # dx # dy # dlq

                pure $ selfIdentity #&& confPreserved #&& scriptPreserved #&& validAction
