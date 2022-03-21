{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ErgoDex.PContracts.POrder
  ( OrderRedeemer(..)
  , OrderAction(..)
  ) where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic, I (I))

import Plutarch
import Plutarch.Prelude
import Plutarch.DataRepr (PDataFields, PIsDataReprInstances(..))
import Plutarch.Builtin  (pforgetData, pasInt)
import Plutarch.Unsafe   (punsafeCoerce)

data OrderAction (s :: S) = Apply | Refund

instance PIsData OrderAction where
  pfromData tx =
    let x = pasInt # pforgetData tx
    in pmatch' x pcon
  pdata x = pmatch x (punsafeCoerce . pdata . pcon')

instance PlutusType OrderAction where
  type PInner OrderAction _ = PInteger

  pcon' Apply = 0
  pcon' Refund = 1

  pmatch' x f =
    pif (x #== 0) (f Apply) (f Refund)

newtype OrderRedeemer (s :: S)
  = OrderRedeemer
      ( Term
          s
          ( PDataRecord
              '[ "poolInIx"    ':= PInteger
               , "orderInIx"   ':= PInteger
               , "rewardOutIx" ':= PInteger
               , "action"      ':= OrderAction
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PMatch, PIsData, PDataFields, PlutusType)
    via PIsDataReprInstances OrderRedeemer
