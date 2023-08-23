{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
-- TODO the PValue instances should go to
-- Plutarc.Api.V1.Value but they
-- depend on quite a bit of
-- stuff in plutarch-extra
{-# OPTIONS_GHC -Wno-orphans #-}

module PExtra.API (
    PAssetClass (..),
    PMonoid,
    --valueLTE,
    assetClass,
    assetClassValue,
    assetClassValueOf,
    getContinuingOutputs,
    tletUnwrap,
    findOwnInput,
    --convertBackValue,
    mustPayToPubKey,
    ptryFromData,
    pValueLength
) where

import qualified GHC.Generics as GHC

import Plutarch.Prelude
import Plutarch.DataRepr
import Plutarch.Lift

import Plutarch.Api.V2 (
    PAddress (PAddress),
    PPubKeyHash,
    PScriptContext (..),
    PScriptPurpose (PSpending),
    PTxId (..),
    PTxInInfo (..),
    PTxInfo (..),
    PTxOut (..),
    PTxOutRef (..),
    PMap (..)
 )
import Plutarch

import Plutarch.Api.V1 (
    PCredential (PPubKeyCredential),
    PCurrencySymbol,
    PTokenName,
    PValue (..)
 )

import qualified Plutarch.Api.V1.Value as PlutarchValue
import Plutarch.DataRepr (PDataFields, DerivePConstantViaData)
import Plutarch.List (pconvertLists)
import Plutarch.Extra.TermCont

import PExtra.Monadic (tcon, tlet, tletField, tmatchField)
import qualified PlutusLedgerApi.V1.Value   as Value

tletUnwrap :: (PIsData a) => Term s (PAsData a) -> TermCont @r s (Term s a)
tletUnwrap = tlet . pfromData

getByteString :: Term s (PTxId :--> PByteString)
getByteString = phoistAcyclic $
    plam $ \txid ->
        pmatch txid $ \(PTxId txid') ->
            pfromData $ pfield @"_0" # txid'

newtype PAssetClass (s :: S)
    = PAssetClass
        ( Term
            s
            ( PDataRecord
                '[ "currencySymbol" ':= PCurrencySymbol
                 , "tokenName" ':= PTokenName
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving anyclass (PIsData, PDataFields, PlutusType, PTryFrom PData)

instance DerivePlutusType PAssetClass where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PAssetClass where type PLifted PAssetClass = Value.AssetClass
deriving via (DerivePConstantViaData Value.AssetClass PAssetClass) instance (PConstantDecl Value.AssetClass)

instance PEq PAssetClass where
    a #== b =
        pletFields @'["currencySymbol", "tokenName"] a $ \a' ->
            pletFields @'["currencySymbol", "tokenName"] b $ \b' ->
                let a1 = pfromData $ getField @"tokenName" a'
                    a2 = pfromData $ getField @"tokenName" b'

                    b1 = pfromData $ getField @"currencySymbol" a'
                    b2 = pfromData $ getField @"currencySymbol" b'
                 in (a1 #== a2) #&& (b1 #== b2)

type PAssetClass' = PPair PCurrencySymbol PTokenName
type PMonoid (a :: PType) = forall s. Monoid (Term s a)

assetClass :: Term s (PCurrencySymbol :--> PTokenName :--> PAssetClass)
assetClass = phoistAcyclic $
    plam $ \cs t ->
        pcon $
            PAssetClass $
                pdcons
                    # pdata cs #$ pdcons
                    # pdata t #$ pdnil

assetClassValue :: Term s (PAssetClass :--> PInteger :--> PValue _ _)
assetClassValue = phoistAcyclic $
    plam $ \ac' n -> unTermCont $ do
        ac <- pletFieldsC @'["currencySymbol", "tokenName"] ac'
        let
            cs = getField @"currencySymbol" ac
            tn = getField @"tokenName" ac
        pure $ PlutarchValue.psingleton # cs # tn # n

assetClassValueOf :: Term s (PValue _ _ :--> PAssetClass :--> PInteger)
assetClassValueOf = phoistAcyclic $
    plam $ \v ac' -> unTermCont $ do
        ac <- pletFieldsC @'["currencySymbol", "tokenName"] ac'
        let
            cs = getField @"currencySymbol" ac
            tn = getField @"tokenName" ac
        pure $ PlutarchValue.pvalueOf # v # cs # tn

getContinuingOutputs :: Term s (PScriptContext :--> PBuiltinList PTxOut)
getContinuingOutputs = phoistAcyclic $
    plam $ \sc -> unTermCont $ do
        txinfo <- tletField @"txInfo" sc
        let outs = pfield @"outputs" # txinfo
        pure $
            pmatch (findOwnInput # sc) $ \case
                PJust te -> unTermCont $ do
                    let 
                      resolved = pfield @"resolved" # te
                      outAdr   = pfield @"address"  # resolved
                    pure $ pfilter # (matches # outAdr) # outs
                PNothing -> ptraceError "can't get any continuing outputs"
  where
    matches :: Term s (PAddress :--> PTxOut :--> PBool)
    matches = phoistAcyclic $
        plam $ \adr txOut -> unTermCont $ do
            let outAdr = pfield @"address" # txOut
            pure $ adr #== outAdr

-- works with plutarch v2
pfindOwnInput :: Term s (PBuiltinList PTxInInfo :--> PTxOutRef :--> PMaybe PTxInInfo)
pfindOwnInput = phoistAcyclic $
  plam $ \inputs outRef ->
    pfind # (matches # outRef) # inputs
  where
    matches :: Term s (PTxOutRef :--> PTxInInfo :--> PBool)
    matches = phoistAcyclic $
      plam $ \outref txininfo ->
        outref #== pfield @"outRef" # txininfo

findOwnInput :: Term s (PScriptContext :--> PMaybe PTxInInfo)
findOwnInput = phoistAcyclic $
    plam $ \sc -> unTermCont $ do
        ctx <- pletFieldsC @["txInfo", "purpose"] sc
        pmatchC (getField @"purpose" ctx) >>= \case
            PSpending outRef' -> do
                let outRef = pfield @"_0" # outRef'
                    inputs = pfield @"inputs" # (getField @"txInfo" ctx)
                pure $ pfindOwnInput # inputs # outRef
            _ ->
                pure $ (pcon PNothing)
  where
    matches :: Term s (PTxOutRef :--> PTxInInfo :--> PBool)
    matches = phoistAcyclic $
        plam $ \outref txininfo -> unTermCont $ do
            PTxOutRef outref' <- pmatchC outref
            outRefId <- tletField @"id" outref'
            PTxInInfo txininfo' <- pmatchC txininfo
            PTxOutRef inOutRef <- tmatchField @"outRef" txininfo'
            inOutRefId <- tletField @"id" inOutRef
            pure $
                outRefId #== inOutRefId

mustPayToPubKey :: Term s (PPubKeyHash :--> PValue _ _ :--> PScriptContext :--> PBool)
mustPayToPubKey = plam $ \pk vl ctx ->
    ptraceIfFalse "mustPayToPubKey" $
        unTermCont $ do
            PScriptContext te <- pmatchC ctx
            PTxInfo txinfo <- tmatchField @"txInfo" te
            outputs <- tletField @"outputs" txinfo
            (outputs' :: Term s (PList PTxOut)) <- tlet $ pconvertLists # outputs
            pure $
                pany # (outputPaysTo # vl # pk) # outputs'

outputPaysTo :: Term s (PValue _ _ :--> PPubKeyHash :--> PTxOut :--> PBool)
outputPaysTo = plam $ \vl pkh txout -> unTermCont $ do
    PTxOut txout' <- pmatchC txout
    PAddress adr <- tmatchField @"address" txout'
    pure $
        pmatch (pfromData $ pfield @"credential" # adr) $ \case
            PPubKeyCredential cred -> unTermCont $ do
                pkhOut <- tletField @"_0" cred
                vl' <- tletField @"value" txout'
                pure $
                    (vl' #<= vl) #&& (pkhOut #== pkh)
            _ -> pcon PFalse

convertAC' :: Term s (PAssetClass :--> PAssetClass')
convertAC' = phoistAcyclic $
    plam $ \ac -> unTermCont $ do
        cs <- tletField @"currencySymbol" ac
        tn <- tletField @"tokenName" ac
        tcon $ PPair cs tn

ptryFromData :: forall a s. PTryFrom PData (PAsData a) => Term s PData -> Term s (PAsData a)
ptryFromData x = unTermCont $ fst <$> tcont (ptryFrom @(PAsData a) x)

-- return correct qty of tokens in value
pValueLength :: Term s (PValue _ _ :--> PInteger)
pValueLength = plam $ \val -> outer #$ pto . pto $ val
  where
    outer :: ClosedTerm (PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap k PTokenName PInteger))) :--> PInteger)
    outer = pfix #$ plam $ \self m ->
      pmatch m $ \case
        PCons x xs -> (plength # (pto . pfromData $ psndBuiltin # x)) + self # xs
        PNil -> pconstant 0