{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.PContracts.POrder (
    OrderRedeemer (..),
    OrderAction (..),
) where

import qualified GHC.Generics       as GHC
import           Generics.SOP       (Generic, I (I))

import qualified Plutarch.Prelude             as P
import           Plutarch
import           Plutarch.Internal.PlutusType (PInner, PlutusType, pcon',pmatch')
import           Plutarch.Builtin             (PIsData (..), pasInt, pforgetData)
import           Plutarch.DataRepr            (DerivePConstantViaData (..), PDataFields)
import           Plutarch.Lift
import           Plutarch.Prelude
import           Plutarch.Unsafe              (punsafeCoerce)

import qualified ErgoDex.Contracts.Proxy.Order as O

data OrderAction (s :: S) = Apply | Refund

instance PIsData OrderAction where
    pfromDataImpl tx =
        let x = pasInt # pforgetData tx
         in pmatch' x pcon
    pdataImpl x = pmatch x (punsafeCoerce . pdata . pcon')

instance PlutusType OrderAction where
    type PInner OrderAction = PInteger

    pcon' Apply  = 0
    pcon' Refund = 1

    pmatch' x f =
        pif (x #== 0) (f Apply) (f Refund)

newtype OrderRedeemer (s :: S)
    = OrderRedeemer
        ( Term
            s
            ( PDataRecord
                '[ "poolInIx" ':= PInteger
                 , "orderInIx" ':= PInteger
                 , "rewardOutIx" ':= PInteger
                 , "action" ':= OrderAction
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType OrderRedeemer where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl OrderRedeemer where type PLifted OrderRedeemer = O.OrderRedeemer
deriving via (DerivePConstantViaData O.OrderRedeemer OrderRedeemer) instance (PConstantDecl O.OrderRedeemer)
