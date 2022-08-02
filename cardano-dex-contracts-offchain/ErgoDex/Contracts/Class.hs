module ErgoDex.Contracts.Class where

-- Switch between erased and well-typed contracts back and forth
class UnliftErased a ea where
    lift :: a -> ea
    unlift :: ea -> a
