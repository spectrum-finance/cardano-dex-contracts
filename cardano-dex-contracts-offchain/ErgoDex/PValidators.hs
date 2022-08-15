module ErgoDex.PValidators (
    poolValidator,
    swapValidator,
    depositValidator,
    redeemValidator,
) where

import           Codec.Serialise
import qualified Data.ByteString.Lazy   as BSL
import           Control.Monad.IO.Class

import           Paths_cardano_dex_contracts_offchain

import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Address       (scriptHashAddress)
import Plutus.V1.Ledger.Scripts
import qualified PlutusTx.Builtins    as Builtins

poolValidatorDataFileName :: String
poolValidatorDataFileName = "pool.uplc"

poolValidator :: (MonadIO m) => m Validator
poolValidator = readValidatorFromFile poolValidatorDataFileName

swapValidatorDataFileName :: String
swapValidatorDataFileName = "swap.uplc"

swapValidator :: (MonadIO m) => m Validator
swapValidator = readValidatorFromFile swapValidatorDataFileName

depositValidatorDataFileName :: String
depositValidatorDataFileName = "deposit.uplc"

depositValidator :: (MonadIO m) => m Validator
depositValidator = readValidatorFromFile depositValidatorDataFileName

redeemValidatorDataFileName :: String
redeemValidatorDataFileName = "redeem.uplc"

redeemValidator :: (MonadIO m) => m Validator
redeemValidator = readValidatorFromFile redeemValidatorDataFileName

readValidatorFromFile :: (MonadIO m) => String -> m Validator
readValidatorFromFile dataFieldName = do
    path  <- liftIO $ getDataFileName dataFieldName
    bytes <- liftIO $ BSL.readFile path
    pure $ deserialise bytes