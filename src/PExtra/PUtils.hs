module PExtra.PUtils where

import Plutarch.Api.V1
import Plutus.V1.Ledger.Api
import qualified Plutus.V1.Ledger.Interval as Interval
import qualified Plutus.V1.Ledger.Value as Value
import Plutarch.Prelude
import ErgoDex.PContracts.PSwap
import qualified Ledger.Typed.Scripts.Validators as LV
import ErgoDex.PContracts.PPool
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

pPPoolConfig :: Term s PoolConfig
pPPoolConfig = phoistAcyclic $
    pcon $
      PoolConfig $
        pdcons
          # pdata acpoolNft #$ pdcons
          # pdata acBase #$ pdcons
          # pdata acquote #$ pdcons
          # pdata acpoolLp #$ pdcons
          # pdata 1 #$ pdnil

pPPoolSwapRedeemer :: Term s PoolRedeemer
pPPoolSwapRedeemer = phoistAcyclic $
   pcon $
     PoolRedeemer $
       pdcons
         # pdata (pcon Swap) #$ pdcons
         # pdata 0 #$ pdnil

pPPoolDepositRedeemer :: Term s PoolRedeemer
pPPoolDepositRedeemer = phoistAcyclic $
   pcon $
     PoolRedeemer $
       pdcons
         # pdata (pcon Deposit) #$ pdcons
         # pdata 0 #$ pdnil

pPPoolRedeemRedeemer :: Term s PoolRedeemer
pPPoolRedeemRedeemer = phoistAcyclic $
   pcon $
     PoolRedeemer $
       pdcons
         # pdata (pcon Redeem) #$ pdcons
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

acpoolLp :: Term s PAssetClass
acpoolLp = phoistAcyclic $
    pcon $
      PAssetClass $
        pdcons
          # pdata (pconstant lpCs) #$ pdcons
          # pdata (pconstant lpTn) #$ pdnil

info :: TxInfo
info =
  TxInfo
    { txInfoInputs = [pPoolIn, pOrderIn]
    , txInfoOutputs = [pPoolOut]
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

pPoolOut :: TxOut
pPoolOut =
  TxOut
    { txOutAddress   = Address (ScriptCredential mkPoolValidator) Nothing
    , txOutValue     = pPoolValueBefore
    , txOutDatumHash = Just datum
    }

pOrderIn :: TxInInfo
pOrderIn =
  TxInInfo
    { txInInfoOutRef = ref
    , txInInfoResolved = pOrderOut
    }

pOrderOut :: TxOut
pOrderOut =
  TxOut
    { txOutAddress   = Address (ScriptCredential mkPoolValidator) Nothing
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

pPoolSwapOut :: TxOut
pPoolSwapOut =
  TxOut
    { txOutAddress   = Address (PubKeyCredential pPubKeyHashReward) Nothing
    , txOutValue     = pRewardValue
    , txOutDatumHash = Just datum
    }

nftCs :: CurrencySymbol
nftCs = "805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487da"

nftTn :: TokenName
nftTn = "4e46545f546f6b656e5f6e65775f706f6f6c5f320a"

baseCs :: CurrencySymbol
baseCs = "805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487da"

baseTn :: TokenName
baseTn = "415f546f6b656e5f6e65775f706f6f6c0a"

quoteCs :: CurrencySymbol
quoteCs = "805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487da"

quoteTn :: TokenName
quoteTn = "425f546f6b656e5f6e65775f706f6f6c0a"

lpCs :: CurrencySymbol
lpCs = "805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487da"

lpTn :: TokenName
lpTn = "544f4b454e5f4c505f4e45575f504f4f4c5f320a"

adaCs :: CurrencySymbol
adaCs = ""

adaTn :: TokenName
adaTn = ""

pPoolValueBefore :: Value
pPoolValueBefore = Value.singleton nftCs nftTn 1 <> Value.singleton baseCs baseTn 100 <> Value.singleton quoteCs quoteTn 100 <> Value.singleton lpCs lpTn 100



pOrderValue :: Value
pOrderValue = Value.singleton adaCs adaTn 1000000 <> Value.singleton quoteCs quoteTn 10

pRewardValue :: Value
pRewardValue = Value.singleton adaCs adaTn 1000000 <> Value.singleton quoteCs quoteTn 10

pPubKeyHashReward :: PubKeyHash
pPubKeyHashReward = "d74d26c5029cf290094fce1a0670da7369b9026571dfb977c6fa234f"

-- | Minting a single token
mint :: Value
mint = Value.singleton sym "sometoken" 1

ref :: TxOutRef
ref = TxOutRef "a0" 0

purpose :: ScriptPurpose
purpose = Spending ref

--validator :: ValidatorHash
--validator = "a1"

datum :: DatumHash
datum = "d0"

sym :: CurrencySymbol
sym = "c0"

signatories :: [PubKeyHash]
signatories = [pPubKeyHashReward]

mkPoolValidator = LV.validatorHash $ LV.unsafeMkTypedValidator $ Validator $ compile poolValidator