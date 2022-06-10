module Main(main) where

import Tests.Deposit 
import Tests.Pool 
import Tests.Swap
import Tests.Redeem

import Test.Tasty
import Test.Tasty.HUnit
import           ErgoDex.Contracts.Proxy.Typed.Swap
import           ErgoDex.Contracts.Types
import           ErgoDex.Contracts.Proxy.Typed.Deposit
import           ErgoDex.Contracts.Proxy.Order
import           ErgoDex.Contracts.Proxy.OffChain
import           Cardano.Ledger.Alonzo.TxInfo
import           Cardano.Ledger.Alonzo.Scripts (ExUnits(..))
import qualified Data.ByteString.Short as SBS
import qualified ErgoDex.PValidators as PSc
import           ErgoDex.Contracts.Proxy.Typed.Redeem
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
import           ErgoDex.Contracts.Class
import qualified Plutus.V1.Ledger.Api as PV1
import qualified Plutus.V2.Ledger.Api as PV2
import qualified Ledger.Address as Addr
import           Control.Monad
import           Ledger
import           Ledger.Value
import           PlutusTx.Builtins.Internal
import qualified ErgoDex.Contracts.Typed as ECT
import qualified ErgoDex.Contracts.Pool as ECP
import qualified ErgoDex.Contracts.Proxy.Swap as CPS
import qualified ErgoDex.Contracts.Proxy.Deposit as CPD
import qualified ErgoDex.Contracts.Proxy.Redeem as CPR
import           PlutusTx.IsData.Class
import           Data.Functor
import qualified Data.ByteString.Lazy as LBS
import qualified Codec.Serialise as Codec
import Text.Hex (encodeHex)
-- import Ledger.Scripts (toCardanoAPIData)
import Cardano.Api
    ( scriptDataToJson,
      ScriptDataJsonSchema(ScriptDataJsonDetailedSchema) )
import Cardano.Api.Shelley ( fromPlutusData )
import Cardano.Binary
import Data.Aeson as Json ( encode )
import qualified PlutusTx
import Data.Char (chr)
import Data.ByteString as S (ByteString, unpack)
import ErgoDex.Contracts.OffChain
import Codec.Serialise          (serialise )
import Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import Plutus.V1.Ledger.Scripts (unValidatorScript)
import qualified Data.Text       as T
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy  as LBS
import Ledger.Typed.Scripts.Validators 
import ErgoDex.Contracts.Types
import PlutusTx.AssocMap as Map
import qualified PlutusTx.Builtins     as BI
import           Plutus.V1.Ledger.Api
import ErgoDex.PContracts.PPool as PP
import qualified ErgoDex.PContracts.PDeposit as PD
import Plutarch
import Plutarch.Prelude
import Plutarch.Builtin (pasInt, pasByteStr)
import PExtra.API
import Plutarch.DataRepr
import Plutarch.Api.V1 (mkMintingPolicy, PMintingPolicy(..), mintingPolicySymbol, mkValidator)
import Plutarch.Api.V1.Value (PCurrencySymbol(..), PValue(..))
import Plutarch.Lift
import Plutarch.Api.V1.Contexts
import ErgoDex.PMintingValidators

main :: IO ()
main = do
  let
    lqMP = poolLqMiningValidator 
      (TxOutRef (Plutus.V1.Ledger.Api.TxId $BuiltinByteString $ mkByteString $ T.pack "9951fb173e9f5feacb0fb76579dc83eb55cfd8522232f2a2ec45973ecade6ff6") 1) 
      (TokenName $ BuiltinByteString $ mkByteString $ T.pack "4333745F414441745F6C70") 
      9223372036854775807
    lqV = PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . unMintingPolicyScript $ lqMP :: PlutusScript PlutusScriptV1
  _ <- writeFileTextEnvelope "/home/timofey/development/haskell/cardano-dex-contracts/lq.txt" Nothing lqV

  let
    nftMP = poolNftMiningValidator 
      (TxOutRef (Plutus.V1.Ledger.Api.TxId $BuiltinByteString $ mkByteString $ T.pack "43fa7e5ecd8e6114ca8e26d63139038ee71a5341c3416f58231ef3eae8f47b7b") 0) 
      (TokenName $ BuiltinByteString $ mkByteString $ T.pack "4333745F414441745F6E6674") 
    nftV = PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . unMintingPolicyScript $ nftMP :: PlutusScript PlutusScriptV1
  _ <- writeFileTextEnvelope "/home/timofey/development/haskell/cardano-dex-contracts/nft.txt" Nothing nftV

  pure ()
  -- defaultMain tests

mkByteString :: T.Text -> ByteString
mkByteString input = unsafeFromEither (Hex.decode . T.encodeUtf8 $ input)

unsafeFromEither :: (Show b) => Either b a -> a
unsafeFromEither (Left err)    = Prelude.error ("Err:" ++ show err)
unsafeFromEither (Right value) = value

tests = testGroup "Contracts"
  [ checkPool
  -- , checkPoolRedeemer
  -- , checkRedeem
  -- , checkRedeemIdentity
  -- , checkRedeemIsFair
  -- , checkRedeemRedeemer
  -- , checkDeposit 
  -- , checkDepositChange
  -- , checkDepositRedeemer
  -- , checkDepositIdentity
  -- , checkDepositLq
  -- , checkDepositTokenReward
  -- , checkSwap
  -- , checkSwapRedeemer
  -- , checkSwapIdentity
  ]