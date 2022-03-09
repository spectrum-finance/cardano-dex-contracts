{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ErgoDex.PContracts.POrder
  ( OrderRedeemer(..)
  ) where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic, I (I))

import Plutarch
import Plutarch.Prelude
import Plutarch.DataRepr

newtype OrderRedeemer (s :: S)
  = DepositRedeemer
      ( Term
          s
          ( PDataRecord
              '[ "poolInIx"    ':= PInteger
               , "orderInIx"   ':= PInteger
               , "rewardOutIx" ':= PInteger
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PMatch, PIsData, PDataFields, PlutusType)
    via PIsDataReprInstances OrderRedeemer
