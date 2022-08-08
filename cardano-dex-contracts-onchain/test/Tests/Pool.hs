{-# LANGUAGE OverloadedStrings #-}

module Tests.Pool where

import qualified ErgoDex.PContracts.PPool as PPool
import qualified ErgoDex.Contracts.Pool   as Pool
import ErgoDex.PValidators
import Data.Either

import Eval
import Gen.Utils

import PlutusLedgerApi.V2

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
  [ HH.testPropertyNamed "pool_deposit_is_correct" "a" successPoolDeposit
  , HH.testPropertyNamed "pool_swap_is_correct" "b" successPoolSwap
  , HH.testPropertyNamed "pool_redeem_is_correct" "c" successPoolRedeem
  , HH.testPropertyNamed "pool_redeem_too_much_liquidity_removed" "d" (poolRedeemLqCheck 9 9 9223372036854775797)
  , HH.testPropertyNamed "pool_redeem_liquidity_removed_lq_intact" "e" (poolRedeemLqCheck 19 19 9223372036854775787)
  , HH.testPropertyNamed "pool_redeem_liquidity_intact_lq_removed" "f" (poolRedeemLqCheck 20 20 9223372036854775786)
  , HH.testPropertyNamed "pool_destroy_lq_burnLqInitial" "a1" (poolDestroyCheck (Pool.maxLqCap - Pool.burnLqInitial) (Right ()))
  , HH.testPropertyNamed "pool_destroy_lq_burnLqInitial-1" "a2" (poolDestroyCheck (Pool.maxLqCap - Pool.burnLqInitial + 1) (Right ()))
  , HH.testPropertyNamed "pool_destroy_lq_burnLqInitial+1" "a3" (poolDestroyCheck (Pool.maxLqCap - Pool.burnLqInitial - 1) (Left ()))
  ]

checkPoolRedeemer = testGroup "CheckPoolRedeemer"
  [ HH.testPropertyNamed "fail_if_pool_ix_is_incorrect_deposit" "a4" poolDepositRedeemerIncorrectIx
  , HH.testPropertyNamed "fail_if_pool_action_is_incorrect_deposit_to_swap" "a6" (poolDepositRedeemerIncorrectAction Pool.Swap)
  , HH.testPropertyNamed "fail_if_pool_ix_is_incorrect_swap" "a7" poolSwapRedeemerIncorrectIx
  , HH.testPropertyNamed "fail_if_pool_action_is_incorrect_swap_to_deposit" "a8" (poolSwapRedeemerIncorrectAction Pool.Deposit)
  , HH.testPropertyNamed "fail_if_pool_action_is_incorrect_swap_to_redeem" "a9" (poolSwapRedeemerIncorrectAction Pool.Redeem)
  , HH.testPropertyNamed "fail_if_pool_ix_is_incorrect_redeem" "a10" poolRedeemRedeemerIncorrectIx
  , HH.testPropertyNamed "fail_if_pool_action_is_incorrect_redeem_to_swap" "a11" (poolRedeemRedeemerIncorrectAction Pool.Swap)
  ]

poolDestroyCheck :: Integer -> Either () () -> Property
poolDestroyCheck lqQty expected = property $ do
  let (x, y, nft, lq) = genAssetClasses
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 1
    poolTxIn    = genDTxIn poolTxRef pdh lq lqQty nft 1 5000000
  
  let
    txInfo  = mkDTxInfo poolTxIn
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 Pool.Destroy

    result = eraseBoth $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === expected

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
    (pcfg, pdh) = genPConfig x y nft lq 1
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
    (pcfg, pdh) = genPConfig x y nft lq 1
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
    (pcfg, pdh) = genPConfig x y nft lq 1
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
    (pcfg, pdh) = genPConfig x y nft lq 1
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
    (cfgData, dh) = genSConfig x y nft 100 100 100 pkh 10 1
    orderTxIn     = genSTxIn orderTxRef dh x 10 5000000
    orderTxOut    = genSTxOut dh y 10 5000000 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 1000
    poolTxIn    = genPTxIn poolTxRef pdh x 100 y 10000 lq 9223372036854775797 nft 1 5000000
    poolTxOut   = genPTxOut pdh x 11000 y 9000 lq 9223372036854775797 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 Pool.Swap

    result = eraseRight $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Right ()

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
    (pcfg, pdh) = genPConfig x y nft lq 1000
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
    (pcfg, pdh) = genPConfig x y nft lq 1000
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
    (pcfg, pdh) = genPConfig x y nft lq 1
    poolTxIn    = genPTxIn poolTxRef pdh x 10 y 10 lq 9223372036854775797 nft 1 10000
    poolTxOut   = genPTxOut pdh x 20 y 20 lq 9223372036854775787 nft 1 10000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 Pool.Deposit

    result = eraseRight $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Right ()

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
  let (pcfg, pdh) = genPConfig x y nft lq 1

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
  let (pcfg, pdh) = genPConfig x y nft lq 1

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
