module Tests.Pool where

import qualified ErgoDex.PContracts.PPool as PPool
import qualified ErgoDex.Contracts.Pool   as Pool
import ErgoDex.PValidators

import Eval
import Gen.Utils

import Plutus.V1.Ledger.Api

import Hedgehog

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog as HH

import Gen.Models
import Gen.DepositGen
import Gen.PoolGen


checkPoolDeposit = testGroup "CheckPoolDepositContract"
  [ HH.testProperty "pool_deposit_is_correct" successPoolDeposit
  ]
  
checkPoolDepositRedeemer = testGroup "CheckPoolDepositRedeemer"
  [ HH.testProperty "fail_if_pool_ix_is_incorrect" poolDepositRedeemerIncorrectIx
  , HH.testProperty "fail_if_pool_action_is_incorrect" poolDepositRedeemerIncorrectAction
  ]

successPoolDeposit :: Property
successPoolDeposit = property $ do
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
    purpose = mkPurpose orderTxRef

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
    purpose = mkPurpose orderTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 1 Pool.Deposit

    result = eraseBoth $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Left ()

poolDepositRedeemerIncorrectAction :: Property
poolDepositRedeemerIncorrectAction = property $ do
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
    purpose = mkPurpose orderTxRef

    cxtToData        = toData $ mkContext txInfo purpose
    poolRedeemToData = toData $ mkPoolRedeemer 0 Pool.Swap

    result = eraseBoth $ evalWithArgs (wrapValidator PPool.poolValidatorT) [pcfg, poolRedeemToData, cxtToData]

  result === Left ()