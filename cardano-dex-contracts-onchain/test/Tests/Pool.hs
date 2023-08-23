{-# LANGUAGE OverloadedStrings #-}

module Tests.Pool where

import qualified ErgoDex.PContracts.PPool as PPool
import qualified ErgoDex.Contracts.Pool   as Pool
import ErgoDex.PValidators
import ErgoDex.PConstants
import Data.Either

import Eval
import Gen.Utils

import PlutusLedgerApi.V2
import Plutarch.Api.V2 as PV2
import ErgoDex.PConstants
import ErgoDex.PMintingValidators

import Hedgehog

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog as HH

import Gen.Models
import Gen.DepositGen
import Gen.PoolGen
import Gen.SwapGen
import Gen.RedeemGen
import Gen.DestroyGen

checkPool = testGroup "CheckPoolContract"
  [ HH.testProperty "pool_validator_hash_is_correct" validPoolHash
  , HH.testProperty "pool_change_stake_part_is_correct (correct minting)" successPoolChangeStakePartCorrectMinting
  , HH.testProperty "pool_change_stake_part_is_incorrect (incorrect minting)" failedPoolChangeStakePartIncorrectMinting
  , HH.testProperty "pool_deposit_is_correct" successPoolDeposit
  , HH.testProperty "pool_swap_is_correct" successPoolSwap
  , HH.testProperty "incorrect_final_pool_tokens_qty" incorrectPoolTokensQtyInFinalValue
  , HH.testProperty "pool_swap_insufficient_lq_for_bound" poolSwapInsufficientLiqudityForBound
  , HH.testProperty "pool_redeem_is_correct" successPoolRedeem
  , HH.testProperty "pool_swap_additional_tokens" incorrectPoolSwapAdditionalTokens
  , HH.testProperty "pool_redeem_too_much_liquidity_removed" (poolRedeemLqCheck 9 9 9223372036854775797)
  , HH.testProperty "pool_redeem_liquidity_removed_lq_intact" (poolRedeemLqCheck 19 19 9223372036854775787)
  , HH.testProperty "pool_redeem_liquidity_intact_lq_removed" (poolRedeemLqCheck 20 20 9223372036854775786)
  , HH.testProperty "pool_destroy_lq_burnLqInitial" (poolDestroyCheck (Pool.maxLqCap - Pool.burnLqInitial) (Right ()))
  , HH.testProperty "pool_destroy_lq_burnLqInitial-1" (poolDestroyCheck (Pool.maxLqCap - Pool.burnLqInitial + 1) (Right ()))
  , HH.testProperty "pool_destroy_lq_burnLqInitial+1" (poolDestroyCheck (Pool.maxLqCap - Pool.burnLqInitial - 1) (Left ()))
  , HH.testProperty "pool_destroy_incorrect_pool_input" (poolDestroyCheckIncorrectPoolInput (Pool.maxLqCap - Pool.burnLqInitial))
  ]

checkPoolRedeemer = testGroup "CheckPoolRedeemer"
  [ HH.testProperty "fail_if_pool_ix_is_incorrect_deposit" poolDepositRedeemerIncorrectIx
  , HH.testProperty "fail_if_pool_action_is_incorrect_deposit_to_swap" (poolDepositRedeemerIncorrectAction Pool.Swap)
  , HH.testProperty "fail_if_pool_ix_is_incorrect_swap" poolSwapRedeemerIncorrectIx
  , HH.testProperty "fail_if_pool_action_is_incorrect_swap_to_deposit" (poolSwapRedeemerIncorrectAction Pool.Deposit)
  , HH.testProperty "fail_if_pool_action_is_incorrect_swap_to_redeem" (poolSwapRedeemerIncorrectAction Pool.Redeem)
  , HH.testProperty "fail_if_pool_ix_is_incorrect_redeem" poolRedeemRedeemerIncorrectIx
  , HH.testProperty "fail_if_pool_action_is_incorrect_redeem_to_swap" (poolRedeemRedeemerIncorrectAction Pool.Swap)
  ]

validPoolHash :: Property
validPoolHash = withTests 1 $ property $ do
  let
    actualPoolValidatorHash = PV2.validatorHash poolValidator
  actualPoolValidatorHash === poolValidatorHash

poolDestroyCheck :: Integer -> Either () () -> Property
poolDestroyCheck lqQty expected = property $ do
  let (x, y, nft, lq) = genAssetClasses
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 1 [] 0
    poolTxIn    = genDTxIn poolTxRef pdh lq lqQty nft 1 5000000
  
    txInfo  = mkDTxInfo [poolTxIn]
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 Pool.Destroy

    result = eraseBoth $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === expected

poolDestroyCheckIncorrectPoolInput :: Integer -> Property
poolDestroyCheckIncorrectPoolInput lqQty = property $ do
  let (x, y, nft, lq) = genAssetClasses

  (_, _, fakeNft, _) <- forAll genRandomAssetClasses

  poolTxRef      <- forAll genTxOutRef
  incorrectTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 1 [] 0
    poolTxIn    = genPTxIn poolTxRef pdh x 20 y 20 lq 9223372036854775787 nft 1 5000000
    incorrectPoolTxIn = genDTxIn incorrectTxRef pdh lq lqQty fakeNft 1 5000000 

    txInfo  = mkDTxInfo [incorrectPoolTxIn, poolTxIn]
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 Pool.Destroy

    result = eraseLeft $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Left ()

successPoolRedeem :: Property
successPoolRedeem = property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genRConfig x y lq nft 100 pkh
    orderTxIn     = genRTxIn orderTxRef dh lq 10 5000000
    orderTxOut    = genRTxOut dh x 10 y 10 5000000 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 1 [] 0
    poolTxIn    = genPTxIn poolTxRef pdh x 20 y 20 lq 9223372036854775787 nft 1 5000000
    poolTxOut   = genPTxOut pdh x 10 y 10 lq 9223372036854775797 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 Pool.Redeem

    result = eraseRight $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Right ()

poolRedeemLqCheck :: Integer -> Integer -> Integer -> Property
poolRedeemLqCheck xQty yQty lqOutQty = property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genRConfig x y lq nft 100 pkh
    orderTxIn     = genRTxIn orderTxRef dh lq 10 5000000
    orderTxOut    = genRTxOut dh x 10 y 10 5000000 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 1 [] 0
    poolTxIn    = genPTxIn poolTxRef pdh x 20 y 20 lq 9223372036854775787 nft 1 5000000
    poolTxOut   = genPTxOut pdh x xQty y yQty lq lqOutQty nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 Pool.Redeem

    result = eraseBoth $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Left ()

poolRedeemRedeemerIncorrectIx :: Property
poolRedeemRedeemerIncorrectIx = property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genRConfig x y lq nft 100 pkh
    orderTxIn     = genRTxIn orderTxRef dh lq 10 5000000
    orderTxOut    = genRTxOut dh x 10 y 10 5000000 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 1 [] 0
    poolTxIn    = genPTxIn poolTxRef pdh x 20 y 20 lq 9223372036854775787 nft 1 5000000
    poolTxOut   = genPTxOut pdh x 10 y 10 lq 9223372036854775797 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 1 Pool.Redeem

    result = eraseBoth $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Left ()

poolRedeemRedeemerIncorrectAction :: Pool.PoolAction -> Property
poolRedeemRedeemerIncorrectAction action = property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genRConfig x y lq nft 100 pkh
    orderTxIn     = genRTxIn orderTxRef dh lq 10 5000000
    orderTxOut    = genRTxOut dh x 10 y 10 5000000 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 1 [] 0
    poolTxIn    = genPTxIn poolTxRef pdh x 20 y 20 lq 9223372036854775787 nft 1 5000000
    poolTxOut   = genPTxOut pdh x 10 y 10 lq 9223372036854775797 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 action

    result = eraseBoth $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Left ()

successPoolSwap :: Property
successPoolSwap = property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genSConfig x y nft 995 500000 1 pkh 10 4
    orderTxIn     = genSTxIn orderTxRef dh x 10 3813762
    orderTxOut    = genSTxOut dh y 4 1813762 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 995 [] 0
    poolTxIn    = genPTxIn poolTxRef pdh x 10 y 10 lq 9223372036854775797 nft 1 1000000000
    poolTxOut   = genPTxOut pdh x 20 y 6 lq 9223372036854775797 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 Pool.Swap

    result = eraseRight $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Right ()

incorrectPoolTokensQtyInFinalValue :: Property
incorrectPoolTokensQtyInFinalValue = withTests 1 $ property $ do
  let 
    (x, y, nft, lq) = genAssetClasses
    fakeToken       = genFakeToken
  pkh             <- forAll genPkh
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genSConfig x y nft 995 500000 1 pkh 10 4
    orderTxIn     = genSTxIn orderTxRef dh x 10 3813762
    orderTxOut    = genSTxOut dh y 4 1813762 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 995 [] 0
    poolTxIn    = genPTxIn poolTxRef pdh x 10 y 10 lq 9223372036854775797 nft 1 1000000000
    poolTxOut   = genPTxOutWithIncorrectTokensQty pdh fakeToken 100 x 20 y 6 lq 9223372036854775797 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 Pool.Swap

    result = eraseLeft $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Left ()

incorrectPoolSwapAdditionalTokens :: Property
incorrectPoolSwapAdditionalTokens = property $ do
  let (x, y, nft, lq) = genAssetClasses
  (tokenA, _, _, _) <- forAll genRandomAssetClasses
  pkh             <- forAll genPkh
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genSConfig x y nft 995 500000 1 pkh 10 4
    orderTxIn     = genSTxIn orderTxRef dh x 10 3813762
    orderTxOut    = genSTxOut dh y 4 1813762 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 995 [] 0
    poolTxIn    = genPTxIn poolTxRef pdh x 10 y 10 lq 9223372036854775797 nft 1 1000000000
    poolTxOut   = genPTxOutWithAdditionalTokens pdh x 20 y 6 lq 9223372036854775797 nft 1 3000000 tokenA 10
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 Pool.Swap

    result = eraseLeft $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Left ()

poolSwapInsufficientLiqudityForBound :: Property
poolSwapInsufficientLiqudityForBound = property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genSConfig x y nft 995 500000 1 pkh 10 4
    orderTxIn     = genSTxIn orderTxRef dh x 10 3813762
    orderTxOut    = genSTxOut dh y 4 1813762 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 995 [] 21
    poolTxIn    = genPTxIn poolTxRef pdh x 10 y 10 lq 9223372036854775797 nft 1 1000000000
    poolTxOut   = genPTxOut pdh x 20 y 6 lq 9223372036854775797 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 Pool.Swap

    result = eraseLeft $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Left ()

poolSwapRedeemerIncorrectIx :: Property
poolSwapRedeemerIncorrectIx = property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genSConfig x y nft 100 100 100 pkh 10 1
    orderTxIn     = genSTxIn orderTxRef dh x 10 5000000
    orderTxOut    = genSTxOut dh y 10 5000000 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 1000 [] 0
    poolTxIn    = genPTxIn poolTxRef pdh x 100 y 10000 lq 9223372036854775797 nft 1 5000000
    poolTxOut   = genPTxOut pdh x 11000 y 9000 lq 9223372036854775787 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 1 Pool.Swap

    result = eraseBoth $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Left ()

poolSwapRedeemerIncorrectAction :: Pool.PoolAction -> Property
poolSwapRedeemerIncorrectAction action = property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genSConfig x y nft 100 100 100 pkh 10 1
    orderTxIn     = genSTxIn orderTxRef dh x 10 5000000
    orderTxOut    = genSTxOut dh y 10 5000000 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 1000 [] 0
    poolTxIn    = genPTxIn poolTxRef pdh x 100 y 10000 lq 9223372036854775797 nft 1 5000000
    poolTxOut   = genPTxOut pdh x 11000 y 9000 lq 9223372036854775787 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 action

    result = eraseBoth $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Left ()

successPoolDeposit :: Property
successPoolDeposit = property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genDConfig x y nft lq 2 pkh 10000
    orderTxIn     = genTxIn orderTxRef dh x 10 y 10 10004
    orderTxOut    = genTxOut dh lq 10 10000 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 1 [] 0
    poolTxIn    = genPTxIn poolTxRef pdh x 10 y 10 lq 9223372036854775797 nft 1 10000
    poolTxOut   = genPTxOut pdh x 20 y 20 lq 9223372036854775787 nft 1 10000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 Pool.Deposit

    result = eraseRight $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Right ()

successPoolChangeStakePartCorrectMinting :: Property
successPoolChangeStakePartCorrectMinting = property $ do
  let (x, y, nft, lq) = genAssetClasses
  
  stakeAdminPkh  <- forAll genPkh
  newPkhForSC    <- forAll genPkh
  let
    previousSc = Just $ StakingHash (PubKeyCredential stakeAdminPkh)
    newSc      = Just $ StakingHash (PubKeyCredential newPkhForSC)
    mintingCS  = CurrencySymbol $ getScriptHash $ scriptHash (unMintingPolicyScript (poolStakeChangeMintPolicyValidator nft [stakeAdminPkh] 1))

  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 1 [mintingCS] 0
    poolTxIn    = genPTxInWithSC poolTxRef previousSc pdh x 10 y 10 lq 9223372036854775797 nft 1 10000
    poolTxOut   = genPTxOutWithSC pdh newSc x 10 y 10 lq 9223372036854775797 nft 1 10000

    scMintAssetClass = mkAssetClass mintingCS poolStakeChangeMintTokenName

    mintValue = mkValue scMintAssetClass 1

    txInfo  = mkTxInfoWithSignaturesAndMinting [poolTxIn] poolTxOut [stakeAdminPkh] mintValue
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 Pool.ChangeStakingPool

    result = eraseRight $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Right ()

failedPoolChangeStakePartIncorrectMinting :: Property
failedPoolChangeStakePartIncorrectMinting = property $ do
  let (x, y, nft, lq) = genAssetClasses
  
  stakeAdminPkh <- forAll genPkh
  newPkhForSC     <- forAll genPkh
  let 
    previousSc = Just $ StakingHash (PubKeyCredential stakeAdminPkh)
    newSc      = Just $ StakingHash (PubKeyCredential newPkhForSC)
    mintingCS  = CurrencySymbol $ getScriptHash $ scriptHash (unMintingPolicyScript (poolStakeChangeMintPolicyValidator nft [stakeAdminPkh] 1))
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, previousPdh) = genPConfig x y nft lq 1 [mintingCS] 0

    (_, newPdh) = genPConfig x y nft lq 1 [] 0
    poolTxIn    = genPTxInWithSC poolTxRef previousSc previousPdh x 10 y 10 lq 9223372036854775797 nft 1 10000
    poolTxOut   = genPTxOutWithSC newPdh newSc x 10 y 10 lq 9223372036854775797 nft 1 10000
  
    txInfo  = mkTxInfoWithSignaturesAndMinting [poolTxIn] poolTxOut [stakeAdminPkh] mempty
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 Pool.ChangeStakingPool

    result = eraseLeft $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Left ()

poolDepositRedeemerIncorrectIx :: Property
poolDepositRedeemerIncorrectIx = property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genDConfig x y nft lq 2 pkh 1
    orderTxIn     = genTxIn orderTxRef dh x 10 y 10 1000000
    orderTxOut    = genTxOut dh lq 10 (1000000 - 300) pkh
  
  poolTxRef <- forAll genTxOutRef
  let (pcfg, pdh) = genPConfig x y nft lq 1 [] 0

  let
    poolTxIn    = genPTxIn poolTxRef pdh x 10 y 10 lq 9223372036854775797 nft 1 1000000
    poolTxOut   = genPTxOut pdh x 20 y 20 lq 9223372036854775787 nft 1 1000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 1 Pool.Deposit

    result = eraseBoth $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Left ()

poolDepositRedeemerIncorrectAction :: Pool.PoolAction -> Property
poolDepositRedeemerIncorrectAction action = property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genDConfig x y nft lq 2 pkh 1
    orderTxIn     = genTxIn orderTxRef dh x 10 y 10 1000000
    orderTxOut    = genTxOut dh lq 10 (1000000 - 300) pkh
  
  poolTxRef <- forAll genTxOutRef
  let (pcfg, pdh) = genPConfig x y nft lq 1 [] 0

  let
    poolTxIn    = genPTxIn poolTxRef pdh x 10 y 10 lq 9223372036854775797 nft 1 1000000
    poolTxOut   = genPTxOut pdh x 20 y 20 lq 9223372036854775787 nft 1 1000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 action

    result = eraseBoth $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Left ()
