{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ErgoDex.PContracts.PDeposit where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic, I (I))
import Control.Monad.Trans.Cont (cont, runCont)

import Plutarch
import Plutarch.Prelude
import Plutarch.DataRepr
import Plutarch.Api.V1.Contexts
import Plutarch.Api.V1.Scripts
import PExtra.List
import Plutarch.Api.V1.Tuple
import PExtra.API
import PExtra.Ada
import Plutarch.Api.V1 (
  PPubKeyHash,
  PTxInInfo,
  PValue,
  PTxOut(..),
  PCredential (PPubKeyCredential)
 )
import ErgoDex.PContracts.PPool hiding (depositValidator, validDeposit)
import Plutus.V1.Ledger.Scripts
import PExtra.Monadic (tcon, tlet, tletField, tmatch, tmatchField)

newtype PDepositConfig (s :: S) = PDepositConfig
  (
    Term s (
      PDataRecord
      '[ "poolNft"        ':= PAssetClass
       , "tokenA"         ':= PAssetClass
       , "tokenB"         ':= PAssetClass
       , "tokenLp"        ':= PAssetClass
       , "exFee"          ':= PInteger
       , "rewardPkh"      ':= PPubKeyHash
       , "collateralAda"  ':= PInteger
       ]
    )
  )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PMatch, PIsData, PDataFields)
    via (PIsDataReprInstances PDepositConfig)

newtype DepositRedeemer (s :: S)
  = DepositRedeemer
      ( Term
          s
          ( PDataRecord
              '[ "poolIndex"       ':= PInteger
               , "orderIndex"      ':= PInteger
               , "rewardOutputIdx" ':= PInteger
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PMatch, PIsData, PDataFields, PlutusType)
    via PIsDataReprInstances DepositRedeemer

validator :: Validator
validator = Validator $ compile depositValidator

depositValidator :: ClosedTerm (PDepositConfig :--> DepositRedeemer :--> PScriptContext :--> PBool)
depositValidator = plam $ \datumT redeemer contextT -> unTermCont $ do
  ctx           <- tcont $ pletFields @'["txInfo", "purpose"] contextT
  datum         <- tcont $ pletFields @'["tokenA", "tokenB", "tokenLp", "poolNft", "exFee", "rewardPkh", "collateralAda"] datumT
  txInfo        <- tletUnwrap $ hrecField @"txInfo" ctx
  rewardPkh     <- tletUnwrap $ hrecField @"rewardPkh" datum
  tokenA        <- tletUnwrap $ hrecField @"tokenA" datum
  tokenB        <- tletUnwrap $ hrecField @"tokenB" datum
  tokenLp       <- tletUnwrap $ hrecField @"tokenLp" datum
  poolNft       <- tletUnwrap $ hrecField @"poolNft" datum
  exFee         <- tletUnwrap $ hrecField @"exFee" datum
  collateralAda <- tletUnwrap $ hrecField @"collateralAda" datum
  let
    validRefund  = isValidRefund # txInfo # rewardPkh
    validDeposit = isValidDeposit # txInfo # poolNft # tokenA # tokenB # tokenLp # exFee # rewardPkh # collateralAda # redeemer
  pure $ validRefund #|| validDeposit

isValidRefund :: Term s (PTxInfo :--> PPubKeyHash :--> PBool)
isValidRefund = plam $ \txInfo userPubKeyHash -> unTermCont $ do
  let signatories = pfield @"signatories" # txInfo
  pure (pelem # pdata userPubKeyHash # signatories)

isValidDeposit
  :: Term s (
         PTxInfo
    :--> PAssetClass
    :--> PAssetClass
    :--> PAssetClass
    :--> PAssetClass
    :--> PInteger
    :--> PPubKeyHash
    :--> PInteger
    :--> DepositRedeemer
    :--> PBool
  )
isValidDeposit = plam $ \txInfoT poolNft tokenA tokenB tokenLP exFee rewardPkh collateralAda depositRedeemerT -> unTermCont $ do
  txInfo          <- tcont $ pletFields @'["inputs", "outputs"] txInfoT
  depositRedeemer <- tcont $ pletFields @'["poolIndex", "rewardOutputIdx", "orderIndex"] depositRedeemerT
  poolIndex       <- tletUnwrap $ hrecField @"poolIndex" depositRedeemer
  rewardIndex     <- tletUnwrap $ hrecField @"rewardOutputIdx" depositRedeemer
  orderIndex      <- tletUnwrap $ hrecField @"orderIndex" depositRedeemer
  inputs          <- tletUnwrap $ hrecField @"inputs" txInfo
  outputs         <- tletUnwrap $ hrecField @"outputs" txInfo
  poolValue       <- tlet $ getInputValue # inputs # poolIndex # poolNft
  orderValue      <- tlet $ getInputValue # inputs # orderIndex # poolNft
  rewardValue     <- tlet $ getRewardValue # outputs # rewardIndex # rewardPkh
  let
    validFee     = isFairFee # rewardValue # collateralAda
    validInputs  = validInputsQty # inputs
    validReward  = isValidReward # orderValue # rewardValue # poolValue # tokenA # tokenB # tokenLP # exFee # collateralAda
  pure $ validFee #&& validInputs #&& validReward

getInputValue :: Term s (PBuiltinList (PAsData PTxInInfo) :--> PInteger :--> PAssetClass :--> PValue)
getInputValue = plam $ \inputs poolIdx poolNft -> unTermCont $ do
  possiblePoolInput <- tletUnwrap (pelemAt # poolIdx # inputs)
  output            <- tletUnwrap $ pfield @"resolved" # possiblePoolInput
  value             <- tletUnwrap $ pfield @"value" # output
  let nftQty        =  assetClassValueOf # value # poolNft
  pure $ pif (nftQty #== 1)
    value
    (ptraceError "Invalid pool")

getRewardValue :: Term s (PBuiltinList (PAsData PTxOut) :--> PInteger :--> PPubKeyHash :--> PValue)
getRewardValue = plam $ \outputs idx pubkeyHash -> unTermCont $ do
  let output   = (pelemAt # idx # outputs)
  adr          <- tletUnwrap $ pfield @"address" # output
  pure $
      pmatch (pfromData $ pfield @"credential" # adr) $ (\case
        PPubKeyCredential cred -> unTermCont $ do
          pkhOut <- tletUnwrap $ pfield @"_0" # cred
          vl     <- tletUnwrap $ pfield @"value" # output
          pure $ pif (pkhOut #== pubkeyHash)
            vl
            (ptraceError "Invalid reward proposition")
        _ -> ptraceError "Invalid reward proposition")

isFairFee :: Term s (PValue :--> PInteger :--> PBool)
isFairFee = plam $ \rewardValue collateralAda -> unTermCont $ do
  let outputAda = pGetLovelace # rewardValue
  pure $ (collateralAda #<= outputAda)

isValidReward
  :: Term s (
         PValue
    :--> PValue
    :--> PValue
    :--> PAssetClass
    :--> PAssetClass
    :--> PAssetClass
    :--> PInteger
    :--> PInteger
    :--> PBool
  )
isValidReward = plam $ \selfValue rewardValue poolValue tokenA tokenB tokenLP exFee collateralAda -> unTermCont $ do
  let
    minTokenAReward = minTokenReward # selfValue # poolValue # tokenA # tokenLP # exFee # collateralAda
    minTokenBReward = minTokenReward # selfValue # poolValue # tokenB # tokenLP # exFee # collateralAda
    minValue        = pmin # minTokenAReward # minTokenBReward
    lpInReward      = assetClassValueOf # rewardValue # tokenLP
  pure $ pnot # (lpInReward #< minValue)

minTokenReward :: Term s (PValue :--> PValue :--> PAssetClass :--> PAssetClass :--> PInteger :--> PInteger :--> PInteger)
minTokenReward = plam $ \selfValue poolValue token liqToken exFee collateralAda -> unTermCont $ do
  inputReserve <- tlet $ assetClassValueOf # selfValue # token
  inputDeposit <- tlet $ (
        pif (pIsAda # token)
        (inputReserve - exFee - collateralAda)
        inputReserve
      )
  let
    poolTokenReserve = assetClassValueOf # poolValue # token
    poolLiqReserve   = assetClassValueOf # poolValue # liqToken
    minValue = pdiv # (inputDeposit * poolLiqReserve) # poolTokenReserve
  pure $ minValue

validInputsQty :: Term s (PBuiltinList (PAsData PTxInInfo) :--> PBool)
validInputsQty = plam $ \inputs -> unTermCont $ do
  let inputsLength = plength # inputs
  pure $ inputsLength #== 2
