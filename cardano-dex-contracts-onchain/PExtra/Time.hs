module PExtra.Time where

import Plutarch.Prelude
import Plutarch
import Plutarch.Extra.Interval
import Plutarch.Api.V1.Time
import PExtra.Monadic (tmatch)

pmultiply :: Term s (PInteger :--> PPOSIXTime :--> PPOSIXTime)
pmultiply = plam $ \n time -> unTermCont $ do
    PPOSIXTime seconds <- tmatch time
    pure $ pcon $ PPOSIXTime (seconds * n)