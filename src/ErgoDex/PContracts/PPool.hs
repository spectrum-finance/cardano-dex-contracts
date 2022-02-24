{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.PContracts.PPool where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic, I (I))

import Plutarch
import Plutarch.Prelude
import Plutarch.DataRepr
import Plutarch.Lift
import Plutarch.Api.V1.Contexts
import Plutarch.Api.V1 (PTxOut)

import PExtra.Monadic (tcon, tletField)
import PExtra.API

import qualified ErgoDex.Contracts.Pool as P

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

data PoolAction (s :: S) = Deposit | Redeem | Swap
instance PlutusType PoolAction where
  type PInner PoolAction _ = PInteger

  pcon' Deposit = 0
  pcon' Redeem = 1
  pcon' Swap = 2

  pmatch' x f =
    pif (x #== 0) (f Deposit)
      (pif (x #== 1) (f Redeem) (f Swap))

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

poolDiff :: Term s (PoolState :--> PoolState :--> PoolDiff)
poolDiff = plam $ \s0 s1 -> unTermCont $ do
  rx0 <- tletField @"reservesX" s0
  rx1 <- tletField @"reservesX" s1
  ry0 <- tletField @"reservesY" s0
  ry1 <- tletField @"reservesY" s1
  lq0 <- tletField @"liquidity" s0
  lq1 <- tletField @"liquidity" s1
  let
    dx  = rx1 - rx0
    dy  = ry1 - ry0
    dlq = lq0 - lq1 -- pool keeps only the negative part of LQ tokens
  tcon $ PoolDiff
     $ pdcons @"diffX" @PInteger # pdata dx
    #$ pdcons @"diffY" @PInteger # pdata dy
    #$ pdcons @"diffLq" @PInteger # pdata dlq
     # pdnil

maxLqCap :: Term s PInteger
maxLqCap = pconstant 0x7fffffffffffffff

feeDen :: Term s PInteger
feeDen = pconstant 1000

zero :: Term s PInteger
zero = pconstant 0

readPoolState :: Term s (PoolConfig :--> PTxOut :--> PoolState)
readPoolState = plam $ \conf' out -> unTermCont $ do
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

pmin :: POrd a => Term s (a :--> a :--> a)
pmin = phoistAcyclic $ plam $ \a b -> pif (a #<= b) a b

validDeposit :: Term s (PoolState :--> PoolDiff :--> PBool)
validDeposit = plam $ \state diff -> unTermCont $ do
  rx  <- tletField @"reservesX" state
  ry  <- tletField @"reservesY" state
  lq  <- tletField @"liquidity" state
  dx  <- tletField @"diffX" diff
  dy  <- tletField @"diffY" diff
  dlq <- tletField @"diffLq" diff
  let liquidityUnlocked = pmin # (pdiv # (dx * lq) # rx) # (pdiv # (dy * lq) # ry) -- todo: this allows deposit shrinking attack
  pure $ dlq #<= liquidityUnlocked

validRedeem :: Term s (PoolState :--> PoolDiff :--> PBool)
validRedeem = plam $ \state diff -> unTermCont $ do
  rx  <- tletField @"reservesX" state
  ry  <- tletField @"reservesY" state
  lq  <- tletField @"liquidity" state
  dx  <- tletField @"diffX" diff
  dy  <- tletField @"diffY" diff
  dlq <- tletField @"diffLq" diff
  pure $ lq * rx #<= dx * lq #&& dlq * ry #<= dy * lq

validSwap :: Term s (PoolConfig :--> PoolState :--> PoolDiff :--> PBool)
validSwap = plam $ \conf state diff -> unTermCont $ do
  rx     <- tletField @"reservesX" state
  ry     <- tletField @"reservesY" state
  dx     <- tletField @"diffX" diff
  dy     <- tletField @"diffY" diff
  feeNum <- tletField @"feeNum" conf
  pure $ pif (zero #< dx)
    (-dy * (rx * feeDen + dx * feeNum) #<= ry * dx * feeNum)
    (-dx * (ry * feeDen + dy * feeNum) #<= rx * dy * feeNum)

poolValidator :: Term s (PoolConfig :--> PoolAction :--> PScriptContext :--> PBool)
poolValidator = plam $ \datum redeemer context -> undefined
