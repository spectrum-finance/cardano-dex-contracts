{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.PContracts.PPool where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic, I (I))

import Plutarch
import Plutarch.Prelude
import Plutarch.DataRepr
import Plutarch.Lift
import Plutarch.Api.V1.Contexts
import Plutarch.Api.V1 (PValue(..), PTxOut)

import PExtra.Monadic (tcon, tlet, tletField, tmatch, tmatchField)
import PExtra.API

import qualified ErgoDex.Contracts.Pool as P

newtype PPoolConfig (s :: S) = PPoolConfig
  (
    Term s (
      PDataRecord
      '[ "poolNft"    ':= PAssetClass
       , "poolX"      ':= PAssetClass
       , "poolY"      ':= PAssetClass
       , "poolLq"     ':= PAssetClass
       , "poolFeeNum" ':= PInteger
       ]
    )
  )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PMatch, PIsData, PDataFields)
    via PIsDataReprInstances PPoolConfig

instance PUnsafeLiftDecl PPoolConfig where type PLifted PPoolConfig = P.PoolConfig
deriving via (DerivePConstantViaData P.PoolConfig PPoolConfig) instance (PConstant P.PoolConfig)

data PPoolAction (s :: S) = PDeposit | PRedeem | PSwap
instance PlutusType PPoolAction where
  type PInner PPoolAction _ = PInteger

  pcon' PDeposit = 0
  pcon' PRedeem = 1
  pcon' PSwap = 2

  pmatch' x f =
    pif (x #== 0) (f PDeposit)
      (pif (x #== 1) (f PRedeem) (f PSwap))

newtype PPoolState (s :: S) = PPoolState
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
    (PlutusType, PIsData)
    via PIsDataReprInstances PPoolState

maxLqCap :: Term s PInteger
maxLqCap = pconstant 0x7fffffffffffffff

readPoolState :: Term s (PPoolConfig :--> PTxOut :--> PPoolState)
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
  tcon $ PPoolState
     $ pdcons @"reservesX" @PInteger # x
    #$ pdcons @"reservesY" @PInteger # y
    #$ pdcons @"liquidity" @PInteger # lq
     # pdnil

-- test = pdcons @"foo" @PInteger # 7 #$ pdcons @"bar" @PInteger # 42 # pnil

poolValidator :: Term s (PPoolConfig :--> PPoolAction :--> PScriptContext :--> PUnit)
poolValidator = plam $ \datum redeemer context -> undefined 
