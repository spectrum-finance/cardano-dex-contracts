module ErgoDex.Contracts.Proxy.PUtils where

import Plutarch.Api.V1
import Plutus.V1.Ledger.Api
import qualified Plutus.V1.Ledger.Interval as Interval
import qualified Plutus.V1.Ledger.Value as Value
import Plutarch.Prelude
import ErgoDex.PContracts.PSwap 
import PExtra.API
import Data.Text (Text)
import Plutarch.Evaluate (evaluateScript)
import Plutarch (ClosedTerm, compile)
import Plutus.V1.Ledger.Api (ExBudget)
import Plutus.V1.Ledger.Scripts (Script (unScript), ScriptError, applyArguments)
import UntypedPlutusCore (DeBruijn, DefaultFun, DefaultUni, Program)
import PlutusTx (Data)

eval :: ClosedTerm a -> Either ScriptError (ExBudget, [Text], Program DeBruijn DefaultUni DefaultFun ())
eval x = fmap (\(a, b, s) -> (a, b, unScript s)) . evaluateScript $ compile x

pPSwapRedeemer :: Term s PSwapRedeemer
pPSwapRedeemer = phoistAcyclic $
    pcon $
      PSwapRedeemer $
        pdcons
          # pdata 1 #$ pdcons
          # pdata 0 #$ pdcons
          # pdata 0 #$ pdnil

pPSwapConfig :: Term s PSwapConfig
pPSwapConfig = phoistAcyclic $
    pcon $
      PSwapConfig $
        pdcons
          # pdata acBase #$ pdcons
          # pdata acquote #$ pdcons
          # pdata acpoolNft #$ pdcons

          # pdata 1 #$ pdcons
          # pdata 1 #$ pdcons
          # pdata 1 #$ pdcons

          # pdata (pconstant pPubKeyHashReward) #$ pdcons

          # pdata 1 #$ pdcons
          # pdata 0 #$ pdnil

ctx :: Term s PScriptContext
ctx =
  pconstant
    (ScriptContext info purpose)

acBase :: Term s PAssetClass
acBase = phoistAcyclic $
    pcon $
      PAssetClass $
        pdcons
          # pdata (pconstant baseCs) #$ pdcons
          # pdata (pconstant baseTn) #$ pdnil

acquote :: Term s PAssetClass
acquote = phoistAcyclic $
    pcon $
      PAssetClass $
        pdcons
          # pdata (pconstant quoteCs) #$ pdcons
          # pdata (pconstant quoteTn) #$ pdnil

acpoolNft :: Term s PAssetClass
acpoolNft = phoistAcyclic $
    pcon $
      PAssetClass $
        pdcons
          # pdata (pconstant nftCs) #$ pdcons
          # pdata (pconstant nftTn) #$ pdnil

info :: TxInfo
info =
  TxInfo
    { txInfoInputs = [pPoolIn, pOrderIn]
    , txInfoOutputs = [pRewardOut]
    , txInfoFee = mempty
    , txInfoMint = mint
    , txInfoDCert = []
    , txInfoWdrl = []
    , txInfoValidRange = Interval.always
    , txInfoSignatories = signatories
    , txInfoData = []
    , txInfoId = "b0"
    }

pPoolIn :: TxInInfo
pPoolIn =
  TxInInfo
    { txInInfoOutRef = ref
    , txInInfoResolved = pPoolOut
    }

pOrderIn :: TxInInfo
pOrderIn =
  TxInInfo
    { txInInfoOutRef = ref
    , txInInfoResolved = pOrderOut
    }

pPoolOut :: TxOut
pPoolOut = 
  TxOut
    { txOutAddress   = Address (ScriptCredential validator) Nothing
    , txOutValue     = pPoolValue
    , txOutDatumHash = Just datum
    }

pOrderOut :: TxOut
pOrderOut = 
  TxOut
    { txOutAddress   = Address (ScriptCredential validator) Nothing
    , txOutValue     = pOrderValue
    , txOutDatumHash = Just datum
    }

pRewardOut :: TxOut
pRewardOut = 
  TxOut
    { txOutAddress   = Address (PubKeyCredential pPubKeyHashReward) Nothing
    , txOutValue     = pRewardValue
    , txOutDatumHash = Just datum
    }

nftCs :: CurrencySymbol
nftCs = "123014"

nftTn :: TokenName
nftTn = "123014"

baseCs :: CurrencySymbol
baseCs = "abcdef"

baseTn :: TokenName
baseTn = "abcdef"

quoteCs :: CurrencySymbol
quoteCs = "abcdef123014"

quoteTn :: TokenName
quoteTn = "abcdef123014"

lpCs :: CurrencySymbol
lpCs = "123014abcdef123014"

lpTn :: TokenName
lpTn = "123014abcdef123014"

adaCs :: CurrencySymbol
adaCs = ""

adaTn :: TokenName
adaTn = ""

pPoolValue :: Value
pPoolValue = Value.singleton nftCs nftTn 1 <> Value.singleton baseCs baseTn 100 <> Value.singleton quoteCs quoteTn 100 <> Value.singleton lpCs lpTn 100

pOrderValue :: Value
pOrderValue = Value.singleton adaCs adaTn 1000000 <> Value.singleton quoteCs quoteTn 10

pRewardValue :: Value
pRewardValue = Value.singleton adaCs adaTn 1000000 <> Value.singleton quoteCs quoteTn 10

pPubKeyHashReward :: PubKeyHash
pPubKeyHashReward = "ab01fe235c"

-- | Minting a single token
mint :: Value
mint = Value.singleton sym "sometoken" 1

ref :: TxOutRef
ref = TxOutRef "a0" 0

purpose :: ScriptPurpose
purpose = Spending ref

validator :: ValidatorHash
validator = "a1"

datum :: DatumHash
datum = "d0"

sym :: CurrencySymbol
sym = "c0"

signatories :: [PubKeyHash]
signatories = [pPubKeyHashReward]