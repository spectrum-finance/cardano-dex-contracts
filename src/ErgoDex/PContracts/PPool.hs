{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.PContracts.PPool where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic, I (I))

import Plutarch
import Plutarch.Prelude
import Plutarch.DataRepr
import Plutarch.Api.V1.Contexts
import Plutarch.Api.V1 (PValue(..))

import PExtra.API

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
    (PlutusType, PIsData)
    via PIsDataReprInstances PPoolConfig

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

readPoolState :: Term s (PPoolConfig :--> PValue :--> PPoolState)
readPoolState = plam $ \conf vl -> undefined

poolValidator :: Term s (PPoolConfig :--> PPoolAction :--> PScriptContext :--> PUnit)
poolValidator = plam $ \datum redeemer context -> undefined 
