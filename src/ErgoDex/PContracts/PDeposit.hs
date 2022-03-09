{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ErgoDex.PContracts.PDeposit
  ( DepositConfig(..)
  , depositValidatorT
  ) where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic, I (I))

import Plutarch
import Plutarch.Prelude
import Plutarch.DataRepr
import Plutarch.Api.V1.Contexts
import PExtra.API
import PExtra.Ada
import Plutarch.Api.V1 (PPubKeyHash, PValue)
import PExtra.Monadic  (tlet)
import ErgoDex.PContracts.PApi
import ErgoDex.PContracts.POrder

newtype DepositConfig (s :: S) = DepositConfig
  (
    Term s (
      PDataRecord
      '[ "poolNft"       ':= PAssetClass
       , "tokenA"        ':= PAssetClass
       , "tokenB"        ':= PAssetClass
       , "tokenLp"       ':= PAssetClass
       , "exFee"         ':= PInteger
       , "rewardPkh"     ':= PPubKeyHash
       , "collateralAda" ':= PInteger
       ]
    )
  )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PMatch, PIsData, PDataFields, PlutusType)
    via (PIsDataReprInstances DepositConfig)

depositValidatorT :: ClosedTerm (DepositConfig :--> OrderRedeemer :--> PScriptContext :--> PBool)
depositValidatorT = plam $ \datumT redeemer contextT -> unTermCont $ do
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
    validRefund  = hasValidSignatories # txInfo # rewardPkh
    validDeposit = isValidDeposit # txInfo # poolNft # tokenA # tokenB # tokenLp # exFee # rewardPkh # collateralAda # redeemer
  pure $ validRefund #|| validDeposit

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
    :--> OrderRedeemer
    :--> PBool
  )
isValidDeposit = plam $ \txInfoT poolNft tokenA tokenB tokenLP exFee rewardPkh collateralAda depositRedeemerT -> unTermCont $ do
  txInfo          <- tcont $ pletFields @'["inputs", "outputs"] txInfoT
  depositRedeemer <- tcont $ pletFields @'["poolInIx", "orderInIx", "rewardOutIx"] depositRedeemerT
  poolInIx       <- tletUnwrap $ hrecField @"poolInIx" depositRedeemer
  rewardIndex     <- tletUnwrap $ hrecField @"rewardOutIx" depositRedeemer
  orderInIx      <- tletUnwrap $ hrecField @"orderInIx" depositRedeemer
  inputs          <- tletUnwrap $ hrecField @"inputs" txInfo
  poolValue       <- tlet $ getInputValue # inputs # poolInIx
  orderValue      <- tlet $ getInputValue # inputs # orderInIx
  rewardValue     <- tlet $ getRewardValue # txInfoT # rewardIndex # rewardPkh
  let
    validFee     = isFairFee # rewardValue # collateralAda
    validPoolNft = checkPoolNft # poolValue # poolNft
    validInputs  = checkInputsQty # inputs
    validReward  = isValidReward # orderValue # rewardValue # poolValue # tokenA # tokenB # tokenLP # exFee # collateralAda
  pure $ validPoolNft #&& validInputs #&& validFee #&& validReward
  
isFairFee :: Term s (PValue :--> PInteger :--> PBool)
isFairFee = plam $ \rewardValue collateralAda -> unTermCont $ do
  let outputAda = pGetLovelace # rewardValue
  pure $ collateralAda #<= outputAda

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
  let
    inputDeposit = pif (pIsAda # token)
      (inputReserve - exFee - collateralAda)
      inputReserve
    poolTokenReserve = assetClassValueOf # poolValue # token
    poolLiqReserve   =  maxLqCap - assetClassValueOf # poolValue # liqToken
    minValue = pdiv # (inputDeposit * poolLiqReserve) # poolTokenReserve
  pure minValue
