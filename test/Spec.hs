{-# LANGUAGE OverloadedStrings #-}


module Main(main) where

import           Cardano.Ledger.Alonzo.TxInfo
import           Cardano.Ledger.Alonzo.Scripts (ExUnits(..))
import qualified Data.ByteString.Short as SBS
import qualified Data.Text.Encoding as LE
import qualified Data.ByteString.Base16  as Hex
import Cardano.Api
import qualified Cardano.Binary as CBOR
import qualified Data.ByteString.Lazy as BSL
import           System.Random
import qualified Data.ByteString.Char8  as C
import qualified Data.Text.Encoding      as T
import qualified Data.ByteString.Base16  as Hex

import           PlutusTx.Builtins.Internal
import qualified Plutus.V1.Ledger.Api as PV1
import qualified Plutus.V2.Ledger.Api as PV2
import qualified Ledger.Address as Addr
import           Control.Monad
import           Ledger
import           Ledger.Value
import           PlutusTx.Builtins.Internal
import           PlutusTx.IsData.Class
import           Data.Functor
import qualified Data.ByteString.Lazy as LBS
import qualified Codec.Serialise as Codec
-- import Ledger.Scripts (toCardanoAPIData)
import Cardano.Api
    ( scriptDataToJson,
      ScriptDataJsonSchema(ScriptDataJsonDetailedSchema) )
import Cardano.Api.Shelley ( fromPlutusData )
import Cardano.Binary
import qualified PlutusTx
import Data.Char (chr)
import Data.ByteString as S (ByteString, unpack)
import Codec.Serialise          (serialise )
import Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import Plutus.V1.Ledger.Scripts (unValidatorScript)
import qualified Data.Text       as T
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy  as LBS
import Ledger.Typed.Scripts.Validators 
import PlutusTx.AssocMap as Map
import qualified PlutusTx.Builtins     as BI
import           Plutus.V1.Ledger.Api


import Tests.Deposit 
import Tests.Pool 
import Tests.Swap
import Tests.Redeem

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy  as LBS
import ErgoDex.PMintingValidators
import  Gen.DepositGen 
import Plutus.V1.Ledger.Api
import Ledger.Typed.Scripts.Validators
import Cardano.Api (writeFileTextEnvelope, Error(displayError))
import Codec.Serialise          (serialise )
import Ledger.Typed.Scripts.Validators 
import           Ledger
import           Plutus.V1.Ledger.Api
import Ledger.Typed.Scripts.Validators 
import Cardano.Api
import Text.Hex (encodeHex)
--9223371936849775932
-- 100004999875
-- 1.0000499987500624
main :: IO ()
main = do
  let
    a = sqrt (100000000000^2 + 1000000000^2)
  print a
  print $ (9223372036854775807 - a)
  let
    policy = poolLqMiningValidator (TxOutRef "96726f1fb223433a65cc5ded9a55c649bd1c0668f51880c2ce28a884e796c352" 1) genNftTest 1
    -- scr = forwardingMintingPolicyHash $ unsafeMkTypedValidator $ Validator $ unMintingPolicyScript policy
    rrr = PlutusScriptSerialised $ SBS.toShort $ LBS.toStrict $ serialise $ (unMintingPolicyScript policy) :: PlutusScript PlutusScriptV1
  -- b5dc6f335149a4916994c5be3a6713177309bb84f7f67e3adc29933c
  -- print $ encodeHex $ LBS.toStrict $ serialise $ (unMintingPolicyScript policy)
  result  <- writeFileTextEnvelope "/home/timofey/development/haskell/cardano-dex-contracts/test1.policy" Nothing rrr
  -- print scr
  pure ()