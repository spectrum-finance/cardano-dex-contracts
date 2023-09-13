module ErgoDex.PValidators (
    poolValidator,
    swapValidator,
    depositValidator,
    redeemValidator,
    vestingValidator,
    vestingWithPeriodValidator,
    simpleStakingValidator,
    lockPkhStakingValidator
) where

import           Codec.Serialise
import qualified Data.ByteString.Lazy   as BSL
import           Control.Monad.IO.Class

import           Paths_cardano_dex_contracts_offchain

import qualified Plutus.V2.Ledger.Api as PV2

poolValidatorDataFileName :: String
poolValidatorDataFileName = "pool.uplc"

poolValidator :: (MonadIO m) => m PV2.Validator
poolValidator = readValidatorFromFile poolValidatorDataFileName

swapValidatorDataFileName :: String
swapValidatorDataFileName = "swap.uplc"

swapValidator :: (MonadIO m) => m PV2.Validator
swapValidator = readValidatorFromFile swapValidatorDataFileName

depositValidatorDataFileName :: String
depositValidatorDataFileName = "deposit.uplc"

depositValidator :: (MonadIO m) => m PV2.Validator
depositValidator = readValidatorFromFile depositValidatorDataFileName

redeemValidatorDataFileName :: String
redeemValidatorDataFileName = "redeem.uplc"

redeemValidator :: (MonadIO m) => m PV2.Validator
redeemValidator = readValidatorFromFile redeemValidatorDataFileName

simpleStakingValidatorDataFileName :: String
simpleStakingValidatorDataFileName = "simpleStaking.uplc"

simpleStakingValidator :: (MonadIO m) => m PV2.Validator
simpleStakingValidator = readValidatorFromFile simpleStakingValidatorDataFileName

lockPkhStakingValidatorDataFileName :: String
lockPkhStakingValidatorDataFileName = "stakinWithPkh.uplc"

lockPkhStakingValidator :: (MonadIO m) => m PV2.Validator
lockPkhStakingValidator = readValidatorFromFile lockPkhStakingValidatorDataFileName

vestingValidatorDataFileName :: String
vestingValidatorDataFileName = "vesting.uplc"

vestingValidator :: (MonadIO m) => m PV2.Validator
vestingValidator = readValidatorFromFile vestingValidatorDataFileName

vestingWithPeriodValidatorDataFileName :: String
vestingWithPeriodValidatorDataFileName = "vestingWithPeriod.uplc"

vestingWithPeriodValidator :: (MonadIO m) => m PV2.Validator
vestingWithPeriodValidator = readValidatorFromFile vestingWithPeriodValidatorDataFileName

readValidatorFromFile :: (MonadIO m) => String -> m PV2.Validator
readValidatorFromFile dataFieldName = do
    path  <- liftIO $ getDataFileName dataFieldName
    bytes <- liftIO $ BSL.readFile path
    pure $ deserialise bytes