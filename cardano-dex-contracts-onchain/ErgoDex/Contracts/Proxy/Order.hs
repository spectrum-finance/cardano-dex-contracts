{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.Contracts.Proxy.Order (
    OrderRedeemer (..),
    OrderAction (..),
) where

import qualified PlutusTx
import PlutusTx.Builtins
import PlutusTx.IsData.Class

data OrderAction = Apply | Refund
    deriving (Show)

instance FromData OrderAction where
    {-# INLINE fromBuiltinData #-}
    fromBuiltinData d = matchData' d (\_ _ -> Nothing) (const Nothing) (const Nothing) chooseAction (const Nothing)
      where
        chooseAction i
            | i == 0 = Just Apply
            | i == 1 = Just Refund
            | otherwise = Nothing

instance UnsafeFromData OrderAction where
    {-# INLINE unsafeFromBuiltinData #-}
    unsafeFromBuiltinData = maybe (Prelude.error "") id . fromBuiltinData

instance ToData OrderAction where
    {-# INLINE toBuiltinData #-}
    toBuiltinData a = mkI $ case a of
        Apply -> 0
        Refund -> 1

data OrderRedeemer = OrderRedeemer
    { poolInIx    :: Integer
    , orderInIx   :: Integer
    , rewardOutIx :: Integer
    , action      :: OrderAction
    }
    deriving stock (Show)

PlutusTx.makeIsDataIndexed ''OrderRedeemer [('OrderRedeemer, 0)]
