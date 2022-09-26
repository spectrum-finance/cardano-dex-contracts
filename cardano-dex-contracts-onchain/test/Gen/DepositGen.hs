module Gen.DepositGen where

import Hedgehog

import Gen.Models

import PlutusLedgerApi.V2
import PlutusLedgerApi.V1.Value
import PlutusTx.Builtins.Internal

import qualified ErgoDex.Contracts.Proxy.Deposit as D

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16  as Hex
import qualified Data.Text.Encoding      as E
import qualified Data.Text as T

genNft :: TokenName
genNft = TokenName $ BuiltinByteString $ mkByteString $ T.pack "4e46545f546f6b656e5f6e65775f706f6f6c0a"

genX :: TokenName
genX = TokenName $ BuiltinByteString $ mkByteString $ T.pack "415f546f6b656e5f6e65775f706f6f6c0a"

genY :: TokenName
genY = TokenName $ BuiltinByteString $ mkByteString $ T.pack "425f546f6b656e5f6e65775f706f6f6c0a"

genLQ :: TokenName
genLQ = TokenName $ BuiltinByteString $ mkByteString $ T.pack "6572676f6c6162736c70746f6b656e"

genCS :: CurrencySymbol
genCS = CurrencySymbol $ BuiltinByteString $ mkByteString $ T.pack "805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487da"

mkByteString :: T.Text -> BS.ByteString
mkByteString input = unsafeFromEither (Hex.decode . E.encodeUtf8 $ input)

unsafeFromEither :: (Show b) => Either b a -> a
unsafeFromEither (Left err)    = Prelude.error ("Err:" ++ show err)
unsafeFromEither (Right value) = value

genAssetClasses :: (AssetClass, AssetClass, AssetClass, AssetClass)
genAssetClasses =
  let
    cs  = genCS
    lq  = genLQ
    nft = genNft
    x   = genX
    y   = genY
  in (mkAssetClass cs x, mkAssetClass cs y, mkAssetClass cs nft, mkAssetClass cs lq)

genDConfig :: AssetClass -> AssetClass -> AssetClass -> AssetClass -> Integer -> PubKeyHash -> Integer -> (Data, OutputDatum)
genDConfig x y nft lq fee pkh cFee =
  let 
    config = mkDepositConfig nft x y lq fee pkh cFee
    od     = OutputDatum $ mkDatum config
  in (toData config, od)

genTxIn :: TxOutRef -> OutputDatum -> AssetClass -> Integer -> AssetClass -> Integer -> Integer -> TxInInfo
genTxIn ref od x xQty y yQty adaQty =
  let
    value = mkValues [mkValue x xQty, mkValue y yQty, mkAdaValue adaQty] mempty
    txOut = mkTxOut od value mkDepositValidator
  in mkTxIn ref txOut

genTxOut :: OutputDatum -> AssetClass -> Integer -> Integer -> PubKeyHash -> TxOut
genTxOut od lq lqQty adaQty pkh =
  let
    value = mkValues [mkValue lq lqQty, mkAdaValue adaQty] mempty
  in mkTxOut' od value pkh