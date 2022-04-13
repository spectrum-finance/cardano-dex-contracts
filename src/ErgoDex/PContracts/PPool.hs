{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.PContracts.PPool
  ( PoolConfig(..)
  , PoolAction(..)
  , PoolRedeemer(..)
  , poolValidatorT
  , mkSwapValidatorT
  , mkDepositValidatorT
  , mkRedeemValidatorT
  , merklizedPoolValidatorT
  ) where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic, I (I))

import Plutarch
import Plutarch.Prelude
import Plutarch.DataRepr
import Plutarch.Lift
import Plutarch.Api.V1.Contexts
import Plutarch.Api.V1 (PTxOut, PMaybeData(PDJust))

import PExtra.Monadic (tcon, tlet, tletField, tmatch)
import PExtra.List (pelemAt, pfind)
import PExtra.API

import ErgoDex.PContracts.PData (pget)
import qualified ErgoDex.Contracts.Pool as P

import Plutarch.Builtin (pforgetData, pasInt)
import Plutarch.Unsafe (punsafeCoerce)
import Plutarch.Api.V1.Value (PCurrencySymbol, PValue(..))

import ErgoDex.PContracts.PApi

newtype PoolConfig (s :: S) = PoolConfig
  (
    Term s (
      PDataRecord
        '[ "poolNft" ':= PAssetClass
         , "poolX"   ':= PAssetClass
         , "poolY"   ':= PAssetClass
         , "poolLq"  ':= PAssetClass
         , "feeNum"  ':= PInteger
         ]
    )
  )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PMatch, PIsData, PDataFields)
    via PIsDataReprInstances PoolConfig

instance PUnsafeLiftDecl PoolConfig where type PLifted PoolConfig = P.PoolConfig
deriving via (DerivePConstantViaData P.PoolConfig PoolConfig) instance (PConstant P.PoolConfig)

data PoolAction (s :: S) = Deposit | Redeem | Swap | Destroy

instance PIsData PoolAction where
  pfromData tx =
    let x = pasInt # pforgetData tx
    in pmatch' x pcon
  pdata x = pmatch x (punsafeCoerce . pdata . pcon')

instance PlutusType PoolAction where
  type PInner PoolAction _ = PInteger

  pcon' Deposit = 0
  pcon' Redeem = 1
  pcon' Swap = 2
  pcon' Destroy = 3

  pmatch' x f =
    pif (x #== 0) (f Deposit)
      (pif (x #== 1) (f Redeem)
        (pif (x #== 2) (f Swap) (f Destroy)))

newtype PoolRedeemer (s :: S) = PoolRedeemer
  (
    Term s (
      PDataRecord
        '[ "action" ':= PoolAction
         , "selfIx" ':= PInteger
         ]
    )
  )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via PIsDataReprInstances PoolRedeemer

newtype PoolState (s :: S) = PoolState
  (
    Term s (
      PDataRecord
        '[ "reservesX" ':= PInteger
         , "reservesY" ':= PInteger
         , "liquidity" ':= PInteger
         ]
    )
  )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via PIsDataReprInstances PoolState

newtype PoolDiff (s :: S) = PoolDiff
  (
    Term s (
      PDataRecord
        '[ "diffX"  ':= PInteger
         , "diffY"  ':= PInteger
         , "diffLq" ':= PInteger
         ]
    )
  )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via PIsDataReprInstances PoolDiff

readPoolState :: Term s (PoolConfig :--> PTxOut :--> PoolState)
readPoolState = phoistAcyclic $ plam $ \conf' out -> unTermCont $ do
  value  <- tletField @"value" out
  poolX  <- tletField @"poolX" conf'
  poolY  <- tletField @"poolY" conf'
  poolLq <- tletField @"poolLq" conf'
  let
    x     = pdata $ assetClassValueOf # value # poolX
    y     = pdata $ assetClassValueOf # value # poolY
    negLq = assetClassValueOf # value # poolLq
    lq    = pdata $ maxLqCap - negLq
  tcon $ PoolState
     $ pdcons @"reservesX" @PInteger # x
    #$ pdcons @"reservesY" @PInteger # y
    #$ pdcons @"liquidity" @PInteger # lq
     # pdnil

validDeposit :: Term s (PoolState :--> PInteger :--> PInteger :--> PInteger :--> PBool)
validDeposit = phoistAcyclic $ plam $ \state' dx dy dlq -> unTermCont $ do
  state <- tcont $ pletFields @'["reservesX", "reservesY", "liquidity"] state'
  rx    <- tletUnwrap $ hrecField @"reservesX" state
  ry    <- tletUnwrap $ hrecField @"reservesY" state
  lq    <- tletUnwrap $ hrecField @"liquidity" state
  let liquidityUnlocked = pmin # (pdiv # (dx * lq) # rx) # (pdiv # (dy * lq) # ry) -- todo: this allows deposit shrinking attack
  pure $ dlq #<= liquidityUnlocked

validRedeem :: Term s (PoolState :--> PInteger :--> PInteger :--> PInteger :--> PBool)
validRedeem = phoistAcyclic $ plam $ \state' dx dy dlq -> unTermCont $ do
  state <- tcont $ pletFields @'["reservesX", "reservesY", "liquidity"] state'
  rx    <- tletUnwrap $ hrecField @"reservesX" state
  ry    <- tletUnwrap $ hrecField @"reservesY" state
  lq    <- tletUnwrap $ hrecField @"liquidity" state
  pure $ lq * rx #<= dx * lq #&& dlq * ry #<= dy * lq

validSwap :: Term s (PoolConfig :--> PoolState :--> PInteger :--> PInteger :--> PBool)
validSwap = phoistAcyclic $ plam $ \conf state' dx dy -> unTermCont $ do
  state   <- tcont $ pletFields @'["reservesX", "reservesY"] state'
  rx      <- tletUnwrap $ hrecField @"reservesX" state
  ry      <- tletUnwrap $ hrecField @"reservesY" state
  feeNum  <- tletField @"feeNum" conf
  feeDen' <- tlet feeDen

  dxf <- tlet $ dx * feeNum
  dyf <- tlet $ dy * feeNum
  pure $ pif (zero #< dx)
    (-dy * (rx * feeDen' + dxf) #<= ry * dxf)
    (-dx * (ry * feeDen' + dyf) #<= rx * dyf)

-- Guarantees preservation of pool NFT
findPoolOutput :: Term s (PAssetClass :--> PBuiltinList (PAsData PTxOut) :--> PAsData PTxOut)
findPoolOutput =
  phoistAcyclic $
    plam $ \nft ->
      precList
        (\self x xs ->
          let
            value = pfield @"value" # x
            amt   = assetClassValueOf # value # nft
          in pif (amt #== 1) x (self # xs))
        (const $ ptraceError "Pool output not found")

poolValidatorT :: ClosedTerm (PoolConfig :--> PoolRedeemer :--> PScriptContext :--> PBool)
poolValidatorT = plam $ \conf redeemer' ctx' -> unTermCont $ do
  redeemer  <- tcont $ pletFields @'["action", "selfIx"] redeemer'
  selfIx    <- tletUnwrap $ hrecField @"selfIx" redeemer
  ctx       <- tcont $ pletFields @'["txInfo", "purpose"] ctx'
  txinfo'   <- tletUnwrap $ hrecField @"txInfo" ctx
  txInfo    <- tcont $ pletFields @'["inputs", "outputs"] txinfo'
  inputs    <- tletUnwrap $ hrecField @"inputs" txInfo
  outputs   <- tletUnwrap $ hrecField @"outputs" txInfo

  action <- tletUnwrap $ hrecField @"action" redeemer

  selfIn' <- tlet $ pelemAt # selfIx # inputs
  selfIn  <- tcont $ pletFields @'["outRef", "resolved"] selfIn'
  self    <- tletUnwrap $ hrecField @"resolved" selfIn

  s0  <- tlet $ readPoolState # conf # self
  lq0 <- tletField @"liquidity" s0

  pure $ pmatch action $ \case
    Destroy -> lq0 #<= burnLqInitial -- all tokens except for permanetly locked ones are removed
    _       -> unTermCont $ do
      nft       <- tletField @"poolNft" conf
      successor <- tlet $ pfromData $ findPoolOutput # nft # outputs -- nft is preserved

      PSpending selfRef' <- tmatch (pfromData $ hrecField @"purpose" ctx)
      selfRef            <- tletField @"_0" selfRef'
      selfInRef          <- tletUnwrap $ hrecField @"outRef" selfIn
      let selfIdentity = selfRef #== selfInRef -- self is the output currently validated by this script

      maybeSelfDh    <- tletField @"datumHash" self
      maybeSuccDh    <- tletField @"datumHash" successor
      PDJust selfDh' <- tmatch maybeSelfDh
      PDJust succDh' <- tmatch maybeSuccDh
      selfDh         <- tletField @"_0" selfDh'
      succDh         <- tletField @"_0" succDh'
      let confPreserved = succDh #== selfDh -- config preserved

      s1   <- tlet $ readPoolState # conf # successor
      rx0  <- tletField @"reservesX" s0
      rx1  <- tletField @"reservesX" s1
      ry0  <- tletField @"reservesY" s0
      ry1  <- tletField @"reservesY" s1
      lq1  <- tletField @"liquidity" s1
      let
        dx  = rx1 - rx0
        dy  = ry1 - ry0
        dlq = lq0 - lq1 -- pool keeps only the negative part of LQ tokens

      selfAddr <- tletField @"address" self
      succAddr <- tletField @"address" successor
      let scriptPreserved = succAddr #== selfAddr -- validator preserved

      let
        validAction = pmatch action $ \case
          Deposit -> validDeposit # s0 # dx # dy # dlq
          Redeem  -> validRedeem # s0 # dx # dy # dlq
          Swap    -> validSwap # conf # s0 # dx # dy

      pure $ selfIdentity #&& confPreserved #&& scriptPreserved #&& validAction

mkDepositValidatorT :: Term s PoolConfig -> Term s (PInteger :--> PScriptContext :--> PBool)
mkDepositValidatorT conf = plam $ \poolIx ctx -> unTermCont $ do
  txinfo'   <- tletField @"txInfo" ctx
  txinfo    <- tcont $ pletFields @'["inputs", "outputs", "mint"] txinfo'
  inputs    <- tletUnwrap $ hrecField @"inputs" txinfo
  outputs   <- tletUnwrap $ hrecField @"outputs" txinfo
  selfIn    <- tlet $ pelemAt # poolIx # inputs
  self      <- tletField @"resolved" selfIn
  nft       <- tletField @"poolNft" conf
  successor <- tlet $ pfromData $ findPoolOutput # nft # outputs

  selfValue <- tletField @"value" self
  let selfIdentity = assetClassValueOf # selfValue # nft #== 1

  maybeSelfDh    <- tletField @"datumHash" self
  maybeSuccDh    <- tletField @"datumHash" successor
  PDJust selfDh' <- tmatch maybeSelfDh
  PDJust succDh' <- tmatch maybeSuccDh
  selfDh         <- tletField @"_0" selfDh'
  succDh         <- tletField @"_0" succDh'
  let confPreserved = succDh #== selfDh

  s0   <- tlet $ readPoolState # conf # self
  s1   <- tlet $ readPoolState # conf # successor
  rx0  <- tletField @"reservesX" s0
  rx1  <- tletField @"reservesX" s1
  ry0  <- tletField @"reservesY" s0
  ry1  <- tletField @"reservesY" s1
  lq0  <- tletField @"liquidity" s0
  lq1  <- tletField @"liquidity" s1
  let
    dx  = rx1 - rx0
    dy  = ry1 - ry0
    dlq = lq0 - lq1 -- pool keeps only the negative part of LQ tokens

  selfAddr <- tletField @"address" self
  succAddr <- tletField @"address" successor
  let scriptPreserved = succAddr #== selfAddr

  let validAction = validDeposit # s0 # dx # dy # dlq

  pure $ selfIdentity #&& confPreserved #&& scriptPreserved #&& validAction

mkRedeemValidatorT :: Term s PoolConfig -> Term s (PInteger :--> PScriptContext :--> PBool)
mkRedeemValidatorT conf = plam $ \poolIx ctx -> unTermCont $ do
  txinfo'   <- tletField @"txInfo" ctx
  txinfo    <- tcont $ pletFields @'["inputs", "outputs", "mint"] txinfo'
  inputs    <- tletUnwrap $ hrecField @"inputs" txinfo
  outputs   <- tletUnwrap $ hrecField @"outputs" txinfo
  selfIn    <- tlet $ pelemAt # poolIx # inputs
  self      <- tletField @"resolved" selfIn
  nft       <- tletField @"poolNft" conf
  successor <- tlet $ pfromData $ findPoolOutput # nft # outputs

  selfValue <- tletField @"value" self
  let selfIdentity = assetClassValueOf # selfValue # nft #== 1

  maybeSelfDh    <- tletField @"datumHash" self
  maybeSuccDh    <- tletField @"datumHash" successor
  PDJust selfDh' <- tmatch maybeSelfDh
  PDJust succDh' <- tmatch maybeSuccDh
  selfDh         <- tletField @"_0" selfDh'
  succDh         <- tletField @"_0" succDh'
  let confPreserved = succDh #== selfDh

  s0   <- tlet $ readPoolState # conf # self
  s1   <- tlet $ readPoolState # conf # successor
  rx0  <- tletField @"reservesX" s0
  rx1  <- tletField @"reservesX" s1
  ry0  <- tletField @"reservesY" s0
  ry1  <- tletField @"reservesY" s1
  lq0  <- tletField @"liquidity" s0
  lq1  <- tletField @"liquidity" s1
  let
    dx  = rx1 - rx0
    dy  = ry1 - ry0
    dlq = lq0 - lq1 -- pool keeps only the negative part of LQ tokens

  selfAddr <- tletField @"address" self
  succAddr <- tletField @"address" successor
  let scriptPreserved = succAddr #== selfAddr

  let validAction = validRedeem # s0 # dx # dy # dlq

  pure $ selfIdentity #&& confPreserved #&& scriptPreserved #&& validAction

mkSwapValidatorT :: Term s PoolConfig -> Term s (PInteger :--> PScriptContext :--> PBool)
mkSwapValidatorT conf = plam $ \poolIx ctx -> unTermCont $ do
  txinfo'   <- tletField @"txInfo" ctx
  txinfo    <- tcont $ pletFields @'["inputs", "outputs", "mint"] txinfo'
  inputs    <- tletUnwrap $ hrecField @"inputs" txinfo
  outputs   <- tletUnwrap $ hrecField @"outputs" txinfo
  selfIn    <- tlet $ pelemAt # poolIx # inputs
  self      <- tletField @"resolved" selfIn
  nft       <- tletField @"poolNft" conf
  successor <- tlet $ pfromData $ findPoolOutput # nft # outputs

  selfValue <- tletField @"value" self
  let selfIdentity = assetClassValueOf # selfValue # nft #== 1

  maybeSelfDh    <- tletField @"datumHash" self
  maybeSuccDh    <- tletField @"datumHash" successor
  PDJust selfDh' <- tmatch maybeSelfDh
  PDJust succDh' <- tmatch maybeSuccDh
  selfDh         <- tletField @"_0" selfDh'
  succDh         <- tletField @"_0" succDh'
  let confPreserved = succDh #== selfDh

  s0   <- tlet $ readPoolState # conf # self
  s1   <- tlet $ readPoolState # conf # successor
  rx0  <- tletField @"reservesX" s0
  rx1  <- tletField @"reservesX" s1
  ry0  <- tletField @"reservesY" s0
  ry1  <- tletField @"reservesY" s1
  let
    dx = rx1 - rx0
    dy = ry1 - ry0

  selfAddr <- tletField @"address" self
  succAddr <- tletField @"address" successor
  let scriptPreserved = succAddr #== selfAddr

  let validAction = validSwap # conf # s0 # dx # dy

  pure $ selfIdentity #&& confPreserved #&& scriptPreserved #&& validAction

merklizedPoolValidatorT :: ClosedTerm (PBuiltinList PCurrencySymbol :--> PCurrencySymbol :--> PScriptContext :--> PBool)
merklizedPoolValidatorT = plam $ \allowedActions actionNft ctx -> unTermCont $ do
  txinfo'    <- tletField @"txInfo" ctx
  valueMint' <- tletField @"mint" txinfo'

  PValue valueMint <- tmatch valueMint'

  PJust _ <- tmatch $ pfind # plam (#== actionNft) # allowedActions -- make sure a known minting policy is used

  tlet $ pmatch (pget # actionNft # valueMint) $ \case -- todo: possible vector of attack: Put Map(actionNft -> Map.empty) into value mint?
    PNothing -> pconstant False
    _        -> pconstant True
    