{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.Contracts.PPool where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic, I (I))

import Plutarch
import Plutarch.Prelude
import Plutarch.DataRepr
import Plutarch.Api.V1.Contexts
import Plutarch.Api.V1.Scripts
import Plutarch.Api.V1 (PCurrencySymbol, PTokenName)

type PAssetClass = PTuple PCurrencySymbol PTokenName

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

poolValidator :: Term s (PDatum :--> PRedeemer :--> PScriptContext :--> PUnit)
poolValidator = plam $ \datum redeemer context -> undefined 
