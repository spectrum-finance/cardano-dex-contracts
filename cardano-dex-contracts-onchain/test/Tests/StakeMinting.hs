module Tests.StakeMinting where

import Hedgehog

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog as HH

import qualified ErgoDex.PContracts.PAssets as A
import qualified PlutusLedgerApi.V1         as Plutus
import PlutusLedgerApi.V2
import PlutusLedgerApi.V1.Scripts (getScriptHash)
import PlutusLedgerApi.V1.Contexts
import Plutarch
import Plutarch.Api.V2 (scriptHash)
import Plutarch.Prelude
import PlutusTx.Builtins.Internal

import qualified Data.Text as T

import qualified ErgoDex.Contracts.Pool   as Pool
import ErgoDex.PContracts.PPoolStakeChangeMintPolicy
import ErgoDex.PMintingValidators
import ErgoDex.PConstants

import Eval

import Gen.Models
import Gen.DepositGen hiding (mkByteString)
import Gen.PoolGen
import Gen.SwapGen
import Gen.RedeemGen
import Gen.DestroyGen
import Gen.Utils
import Debug.Trace

checkStakeChangeMintingPolicy = testGroup "StakeMinting"
  [ HH.testProperty "correct_currency_symbol" correctCurrencySymbol
  , HH.testProperty "pool_change_stake_part_is_correct (correct stake part change)" successPoolChangeStakePart
  , HH.testProperty "pool_change_stake_part_is_correct (correct stake changer destroy)" successPoolstakeAdminDestory
  , HH.testProperty "pool_change_stake_part_is_correct (correct stake threshold)" successPoolChangeStakePartWithThreshold
  , HH.testProperty "pool_change_stake_part_is_incorrect (incorrect threshold value)"  failedPoolChangeStakePartThresholdError
  , HH.testProperty "pool_change_stake_part_is_incorrect (incorrect signature)"  failedPoolChangeStakePart
  , HH.testProperty "pool_change_stake_part_is_incorrect (incorrect pool value)" failedPoolChangeStakePartIncorrectFinalValue
  , HH.testProperty "pool_change_stake_part_is_incorrect (incorrect pool datum)" failedPoolChangeStakePartIncorrectFinalDatum
  , HH.testProperty "pool_change_stake_part_is_incorrect (incorrect user input. Same pool datum except of stake admins)" failedPoolChangeStakePartIncorrectInputsFakeDatum
  , HH.testProperty "pool_change_stake_part_is_incorrect (incorrect user input. Attack with user's pool)" failedPoolChangeStakePartIncorrectInputsFakePool
  , HH.testProperty "pool_change_stake_part_is_incorrect (incorrect pool index)" failedPoolChangeStakePartIncorrectPoolIdx
  ]

correctCurrencySymbol :: Property
correctCurrencySymbol = withTests 1 $ property $ do
  let
    stakeAdminPkh  = (PubKeyHash $ BuiltinByteString . mkByteString $ T.pack "61616161616161616161616161616161616161616161616161616161")
    correctCSValue = Plutus.CurrencySymbol $ BuiltinByteString . mkByteString $ T.pack "d7d2d653cc0e13188a9471ce2e1e2ab659629b21f3678056a85cc33c"
    (_, _, nft, _) = genAssetClasses
    origCurSymbol = Plutus.CurrencySymbol $ getScriptHash $ scriptHash (Plutus.unMintingPolicyScript (poolStakeChangeMintPolicyValidator nft [stakeAdminPkh] 1))
  origCurSymbol === correctCSValue

successPoolChangeStakePart :: Property
successPoolChangeStakePart = property $ do
  let (x, y, nft, lq) = genAssetClasses
  
  stakeAdminPkh  <- forAll genPkh
  newPkhForSC    <- forAll genPkh
  userFeePkh     <- forAll genPkh
  let 
    previousSc = Just $ StakingHash (PubKeyCredential stakeAdminPkh)
    newSc      = Just $ StakingHash (PubKeyCredential newPkhForSC)
    mintingCS  = CurrencySymbol $ getScriptHash $ scriptHash (unMintingPolicyScript (poolStakeChangeMintPolicyValidator nft [stakeAdminPkh] 1))

  poolTxRef    <- forAll genTxOutRef
  userFeeTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 1 [mintingCS] 0
    (_, newPdh) = genPConfig x y nft lq 1 [] 0
    feeTxIn     = genUTxIn userFeeTxRef 10 userFeePkh
    poolTxIn    = genPTxInWithSC poolTxRef previousSc pdh x 10 y 10 lq 9223372036854775797 nft 1 10000
    poolTxOut   = genPTxOutWithSC newPdh newSc x 10 y 10 lq 9223372036854775797 nft 1 10000

    scMintAssetClass = mkAssetClass mintingCS poolStakeChangeMintTokenName

    mintValue = mkValue scMintAssetClass 1

    txInfo  = mkTxInfoWithSignaturesAndMinting [poolTxIn, feeTxIn] poolTxOut [stakeAdminPkh] mintValue
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData (0 :: Integer)
    result = eraseRight $ evalWithArgs (wrapMintingValidator (poolStakeChangeMintPolicyValidatorT (pconstant nft) (pconstant [stakeAdminPkh]) (pconstant 1))) [poolRedeemToData, cxtToData]

  result === Right ()

successPoolChangeStakePartWithThreshold :: Property
successPoolChangeStakePartWithThreshold = property $ do
  let (x, y, nft, lq) = genAssetClasses
  
  stakeAdminPkh         <- forAll genPkh
  anotherStakeAdminPkh  <- forAll genPkh

  newPkhForSC    <- forAll genPkh
  userFeePkh     <- forAll genPkh
  let 
    previousSc = Just $ StakingHash (PubKeyCredential stakeAdminPkh)
    newSc      = Just $ StakingHash (PubKeyCredential newPkhForSC)
    mintingCS  = CurrencySymbol $ getScriptHash $ scriptHash (unMintingPolicyScript (poolStakeChangeMintPolicyValidator nft [stakeAdminPkh, anotherStakeAdminPkh] 2))

  poolTxRef    <- forAll genTxOutRef
  userFeeTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 1 [mintingCS] 0
    (_, newPdh) = genPConfig x y nft lq 1 [] 0
    feeTxIn     = genUTxIn userFeeTxRef 10 userFeePkh
    poolTxIn    = genPTxInWithSC poolTxRef previousSc pdh x 10 y 10 lq 9223372036854775797 nft 1 10000
    poolTxOut   = genPTxOutWithSC newPdh newSc x 10 y 10 lq 9223372036854775797 nft 1 10000

    scMintAssetClass = mkAssetClass mintingCS poolStakeChangeMintTokenName

    mintValue = mkValue scMintAssetClass 1

    txInfo  = mkTxInfoWithSignaturesAndMinting [poolTxIn, feeTxIn] poolTxOut [stakeAdminPkh, anotherStakeAdminPkh] mintValue
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData (0 :: Integer)
    result = eraseRight $ evalWithArgs (wrapMintingValidator (poolStakeChangeMintPolicyValidatorT (pconstant nft) (pconstant [stakeAdminPkh, anotherStakeAdminPkh]) (pconstant 2))) [poolRedeemToData, cxtToData]

  result === Right ()

successPoolstakeAdminDestory :: Property
successPoolstakeAdminDestory = property $ do
  let (x, y, nft, lq) = genAssetClasses
  
  stakeAdminPkh <- forAll genPkh
  newPkhForSC   <- forAll genPkh
  userFeePkh    <- forAll genPkh
  let 
    previousSc = Just $ StakingHash (PubKeyCredential stakeAdminPkh)
    newSc      = Just $ StakingHash (PubKeyCredential newPkhForSC)
    mintingCS  = CurrencySymbol $ getScriptHash $ scriptHash (unMintingPolicyScript (poolStakeChangeMintPolicyValidator nft [stakeAdminPkh] 1))
  
  poolTxRef    <- forAll genTxOutRef
  userFeeTxRef <- forAll genTxOutRef
  let
    (pcfg, previousPdh) = genPConfig x y nft lq 1 [mintingCS] 0
    
    (_, newPdh) = genPConfig x y nft lq 1 [] 0
    feeTxIn     = genUTxIn userFeeTxRef 10 userFeePkh
    poolTxIn    = genPTxInWithSC poolTxRef previousSc previousPdh x 10 y 10 lq 9223372036854775797 nft 1 10000
    poolTxOut   = genPTxOutWithSC newPdh newSc x 10 y 10 lq 9223372036854775797 nft 1 10000
  
  let
    txInfo  = mkTxInfoWithSignaturesAndMinting [poolTxIn, feeTxIn] poolTxOut [stakeAdminPkh] mempty
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData (0 :: Integer)

    result = eraseRight $ evalWithArgs (wrapMintingValidator (poolStakeChangeMintPolicyValidatorT (pconstant nft) (pconstant [stakeAdminPkh]) (pconstant 1))) [poolRedeemToData, cxtToData]

  result === Right ()

failedPoolChangeStakePartThresholdError :: Property
failedPoolChangeStakePartThresholdError = property $ do
  let (x, y, nft, lq) = genAssetClasses
  
  stakeAdminPkh         <- forAll genPkh
  anotherStakeAdminPkh  <- forAll genPkh

  newPkhForSC    <- forAll genPkh
  userFeePkh     <- forAll genPkh
  let 
    previousSc = Just $ StakingHash (PubKeyCredential stakeAdminPkh)
    newSc      = Just $ StakingHash (PubKeyCredential newPkhForSC)
    mintingCS  = CurrencySymbol $ getScriptHash $ scriptHash (unMintingPolicyScript (poolStakeChangeMintPolicyValidator nft [stakeAdminPkh, anotherStakeAdminPkh] 2))

  poolTxRef    <- forAll genTxOutRef
  userFeeTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 1 [mintingCS] 0
    feeTxIn     = genUTxIn userFeeTxRef 10 userFeePkh
    poolTxIn    = genPTxInWithSC poolTxRef previousSc pdh x 10 y 10 lq 9223372036854775797 nft 1 10000
    poolTxOut   = genPTxOutWithSC pdh newSc x 10 y 10 lq 9223372036854775797 nft 1 10000

    scMintAssetClass = mkAssetClass mintingCS poolStakeChangeMintTokenName

    mintValue = mkValue scMintAssetClass 1

    txInfo  = mkTxInfoWithSignaturesAndMinting [poolTxIn, feeTxIn] poolTxOut [stakeAdminPkh] mintValue
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData (0 :: Integer)
    result = eraseLeft $ evalWithArgs (wrapMintingValidator (poolStakeChangeMintPolicyValidatorT (pconstant nft) (pconstant [stakeAdminPkh, anotherStakeAdminPkh]) (pconstant 2))) [poolRedeemToData, cxtToData]

  result === Left ()

failedPoolChangeStakePart :: Property
failedPoolChangeStakePart = property $ do
  let (x, y, nft, lq) = genAssetClasses
  
  stakeAdminPkh   <- forAll genPkh
  incorrectPkh    <- forAll genPkh
  newPkhForSC     <- forAll genPkh
  userFeePkh      <- forAll genPkh
  let 
    previousSc = Just $ StakingHash (PubKeyCredential stakeAdminPkh)
    newSc      = Just $ StakingHash (PubKeyCredential newPkhForSC)
    mintingCS  = CurrencySymbol $ getScriptHash $ scriptHash (unMintingPolicyScript (poolStakeChangeMintPolicyValidator nft [stakeAdminPkh] 1))
  
  poolTxRef    <- forAll genTxOutRef
  userFeeTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 1 [mintingCS] 0
    feeTxIn     = genUTxIn userFeeTxRef 10 userFeePkh
    poolTxIn    = genPTxInWithSC poolTxRef previousSc pdh x 10 y 10 lq 9223372036854775797 nft 1 10000
    poolTxOut   = genPTxOutWithSC pdh newSc x 10 y 10 lq 9223372036854775797 nft 1 10000
  
  let
    txInfo  = mkTxInfoWithSignatures [poolTxIn, feeTxIn] [poolTxOut] [incorrectPkh]
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData (0 :: Integer)

    result = eraseLeft $ evalWithArgs (wrapMintingValidator (poolStakeChangeMintPolicyValidatorT (pconstant nft) (pconstant [stakeAdminPkh]) (pconstant 1))) [poolRedeemToData, cxtToData]

  result === Left ()

failedPoolChangeStakePartIncorrectFinalValue :: Property
failedPoolChangeStakePartIncorrectFinalValue = property $ do
  let (x, y, nft, lq) = genAssetClasses
  
  stakeAdminPkh   <- forAll genPkh
  newPkhForSC     <- forAll genPkh
  userFeePkh      <- forAll genPkh
  let 
    previousSc = Just $ StakingHash (PubKeyCredential stakeAdminPkh)
    newSc      = Just $ StakingHash (PubKeyCredential newPkhForSC)
    mintingCS  = CurrencySymbol $ getScriptHash $ scriptHash (unMintingPolicyScript (poolStakeChangeMintPolicyValidator nft [stakeAdminPkh] 1))
  
  poolTxRef    <- forAll genTxOutRef
  userFeeTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 1 [mintingCS] 0
    feeTxIn     = genUTxIn userFeeTxRef 10 userFeePkh
    poolTxIn    = genPTxInWithSC poolTxRef previousSc pdh x 10 y 10 lq 9223372036854775797 nft 1 10000
    poolTxOut   = genPTxOutWithSC pdh newSc x 1 y 1 lq 1 nft 0 10000
  
  let
    txInfo  = mkTxInfoWithSignatures [poolTxIn, feeTxIn] [poolTxOut] [stakeAdminPkh]
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData (0 :: Integer)

    result = eraseLeft $ evalWithArgs (wrapMintingValidator (poolStakeChangeMintPolicyValidatorT (pconstant nft) (pconstant [stakeAdminPkh]) (pconstant 1))) [poolRedeemToData, cxtToData]

  result === Left ()

failedPoolChangeStakePartIncorrectFinalDatum :: Property
failedPoolChangeStakePartIncorrectFinalDatum = property $ do
  let (x, y, nft, lq) = genAssetClasses

  (incorretX, incorrectY, incorrectNft, incorrectlq) <- forAll genRandomAssetClasses
  
  stakeAdminPkh <- forAll genPkh
  newPkhForSC   <- forAll genPkh
  userFeePkh    <- forAll genPkh
  let 
    previousSc = Just $ StakingHash (PubKeyCredential stakeAdminPkh)
    newSc      = Just $ StakingHash (PubKeyCredential newPkhForSC)

    mintingCS    = CurrencySymbol $ getScriptHash $ scriptHash (unMintingPolicyScript (poolStakeChangeMintPolicyValidator nft [stakeAdminPkh] 1))
    newMintingCS = CurrencySymbol $ getScriptHash $ scriptHash (unMintingPolicyScript (poolStakeChangeMintPolicyValidator nft [userFeePkh] 1))

  poolTxRef    <- forAll genTxOutRef
  userFeeTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 1 [mintingCS] 0
    feeTxIn     = genUTxIn userFeeTxRef 10 userFeePkh
    (_, incorrectPdh) = genPConfig incorretX incorrectY incorrectNft incorrectlq 1 [newMintingCS] 100
    poolTxIn    = genPTxInWithSC poolTxRef previousSc pdh x 10 y 10 lq 9223372036854775797 nft 1 10000
    poolTxOut   = genPTxOutWithSC incorrectPdh newSc x 10 y 10 lq 9223372036854775797 nft 1 10000
  
  let
    txInfo  = mkTxInfoWithSignatures [poolTxIn, feeTxIn] [poolTxOut] [userFeePkh]
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData (0 :: Integer)
  
    result = eraseLeft $ evalWithArgs (wrapMintingValidator (poolStakeChangeMintPolicyValidatorT (pconstant nft) (pconstant [userFeePkh]) (pconstant 1))) [poolRedeemToData, cxtToData]

  result === Left ()

failedPoolChangeStakePartIncorrectInputsFakePool :: Property
failedPoolChangeStakePartIncorrectInputsFakePool = property $ do
  let (x, y, nft, lq) = genAssetClasses

  (newX, newY, newNft, newLq) <- forAll genRandomAssetClasses
  
  stakeAdminPkh <- forAll genPkh
  newPkhForSC   <- forAll genPkh
  userPkh       <- forAll genPkh
  let 
    previousSc = Just $ StakingHash (PubKeyCredential stakeAdminPkh)
    newSc      = Just $ StakingHash (PubKeyCredential newPkhForSC)

    mintingCS    = CurrencySymbol $ getScriptHash $ scriptHash (unMintingPolicyScript (poolStakeChangeMintPolicyValidator nft [stakeAdminPkh] 1))
    newMintingCS = CurrencySymbol $ getScriptHash $ scriptHash (unMintingPolicyScript (poolStakeChangeMintPolicyValidator nft [userPkh] 1))
  
  poolTxRef      <- forAll genTxOutRef
  userPoolTxRef  <- forAll genTxOutRef
  let
    (pcfg, originalPD) = genPConfig x y nft lq 1 [mintingCS] 0
    (_, userPd)   = genPConfig newX newY newNft newLq 1 [newMintingCS] 0
    poolTxIn      = genPTxInWithSC poolTxRef previousSc originalPD x 10 y 10 lq 9223372036854775797 nft 1 10000
    userPoolTxIn  = genPTxInWithSC userPoolTxRef previousSc userPd newX 10 newY 10 newLq 9223372036854775797 newNft 1 10000
    poolTxOut     = genPTxOutWithSC originalPD newSc x 10 y 10 lq 9223372036854775797 nft 1 10000
    userPoolTxOut = genPTxOutWithSC userPd newSc newX 10 newY 10 newLq 9223372036854775797 newNft 1 10000
  
    txInfo  = mkTxInfoWithSignatures [userPoolTxIn, poolTxIn] [userPoolTxOut, poolTxOut] [userPkh]
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData (0 :: Integer)
  
    result = eraseLeft $ evalWithArgs (wrapMintingValidator (poolStakeChangeMintPolicyValidatorT (pconstant nft) (pconstant [userPkh]) (pconstant 1))) [poolRedeemToData, cxtToData]

  result === Left ()

-- Inputs: User input with same pool datum (except of stakeAdminPolicy), original pool input
failedPoolChangeStakePartIncorrectInputsFakeDatum :: Property
failedPoolChangeStakePartIncorrectInputsFakeDatum = property $ do
  let (x, y, nft, lq) = genAssetClasses
  
  stakeAdminPkh <- forAll genPkh
  newPkhForSC   <- forAll genPkh
  userFeePkh    <- forAll genPkh
  let 
    previousSc = Just $ StakingHash (PubKeyCredential stakeAdminPkh)
    newSc      = Just $ StakingHash (PubKeyCredential newPkhForSC)

    mintingCS     = CurrencySymbol $ getScriptHash $ scriptHash (unMintingPolicyScript (poolStakeChangeMintPolicyValidator nft [stakeAdminPkh] 1))
    newMintingCS  = CurrencySymbol $ getScriptHash $ scriptHash (unMintingPolicyScript (poolStakeChangeMintPolicyValidator nft [userFeePkh] 1))
  
  poolTxRef    <- forAll genTxOutRef
  userFeeTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh)  = genPConfig x y nft lq 1 [mintingCS] 0
    (_, userPd)  = genPConfig x y nft lq 1 [newMintingCS] 0
    userTxInWithPoolDatum = genUTxInWithDatum userFeeTxRef 10 userPd userFeePkh
    poolTxIn    = genPTxInWithSC poolTxRef previousSc pdh x 10 y 10 lq 9223372036854775797 nft 1 10000
    poolTxOut   = genPTxOutWithSC pdh newSc x 10 y 10 lq 9223372036854775797 nft 1 10000
  
    txInfo  = mkTxInfoWithSignatures [userTxInWithPoolDatum, poolTxIn] [poolTxOut] [stakeAdminPkh]
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData (0 :: Integer)
  
    result = eraseLeft $ evalWithArgs (wrapMintingValidator (poolStakeChangeMintPolicyValidatorT (pconstant nft) (pconstant [stakeAdminPkh]) (pconstant 1))) [poolRedeemToData, cxtToData]

  result === Left ()

failedPoolChangeStakePartIncorrectPoolIdx :: Property
failedPoolChangeStakePartIncorrectPoolIdx = property $ do
  let (x, y, nft, lq) = genAssetClasses
  
  stakeAdminPkh  <- forAll genPkh
  newPkhForSC    <- forAll genPkh
  userFeePkh     <- forAll genPkh
  let 
    previousSc = Just $ StakingHash (PubKeyCredential stakeAdminPkh)
    newSc      = Just $ StakingHash (PubKeyCredential newPkhForSC)
    mintingCS  = CurrencySymbol $ getScriptHash $ scriptHash (unMintingPolicyScript (poolStakeChangeMintPolicyValidator nft [stakeAdminPkh] 1))

  poolTxRef    <- forAll genTxOutRef
  userFeeTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 1 [mintingCS] 0
    (_, newPdh) = genPConfig x y nft lq 1 [] 0
    feeTxIn     = genUTxIn userFeeTxRef 10 userFeePkh
    poolTxIn    = genPTxInWithSC poolTxRef previousSc pdh x 10 y 10 lq 9223372036854775797 nft 1 10000
    poolTxOut   = genPTxOutWithSC newPdh newSc x 10 y 10 lq 9223372036854775797 nft 1 10000

    scMintAssetClass = mkAssetClass mintingCS poolStakeChangeMintTokenName

    mintValue = mkValue scMintAssetClass 1

    txInfo  = mkTxInfoWithSignaturesAndMinting [poolTxIn, feeTxIn] poolTxOut [stakeAdminPkh] mintValue
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData (1 :: Integer)
    result = eraseLeft $ evalWithArgs (wrapMintingValidator (poolStakeChangeMintPolicyValidatorT (pconstant nft) (pconstant [stakeAdminPkh]) (pconstant 1))) [poolRedeemToData, cxtToData]

  result === Left ()