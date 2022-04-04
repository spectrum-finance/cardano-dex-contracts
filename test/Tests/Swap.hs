module Tests.Swap where

import qualified ErgoDex.PContracts.PPool as PPool
import qualified ErgoDex.PContracts.PSwap as PSwap
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
import Gen.SwapGen

checkSwap = testGroup "CheckSwap"
  [ HH.testProperty "correct_swap" successSwap
  ]

checkSwapRedeemer = testGroup "SwapRedeemer"
  [ HH.testProperty "fail_if_poolInIx_is_incorrect" swapIncorrectPoolInIx
  , HH.testProperty "fail_if_orderInIx_is_incorrect" swapIncorrectOrderInIx
  , HH.testProperty "fail_if_rewardOutIx_is_incorrect" swapIncorrectRewardOutIx
  ]

checkSwapIdentity = testGroup "CheckSwapIdentity"
  [ HH.testProperty "fail_if_selfIdentity_is_incorrect" swapSelfIdentity
  , HH.testProperty "fail_if_poolIdentity_is_incorrect" swapPoolIdentity
  ]

-- fair price

-- fair fee
  

successSwap :: Property
successSwap = property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genSConfig x y nft 100 100 100 pkh 10 1
    orderTxIn     = genSTxIn orderTxRef dh x 10 5000000
    orderTxOut    = genSTxOut dh y 10 5000000 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 1
    poolTxIn    = genPTxIn poolTxRef pdh x 100 y 100 lq 9223372036854775797 nft 1 5000000
    poolTxOut   = genPTxOut pdh x 110 y 90 lq 9223372036854775787 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose orderTxRef

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkOrderRedeemer 0 1 1

    result = eraseRight $ evalWithArgs (wrapValidator PSwap.swapValidatorT) [cfgData, orderRedeemToData, cxtToData]

  result === Right ()

swapSelfIdentity :: Property
swapSelfIdentity = property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genSConfig x y nft 100 100 100 pkh 10 1
    orderTxIn     = genSTxIn orderTxRef dh x 10 5000000
    orderTxOut    = genSTxOut dh y 10 5000000 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 1
    poolTxIn    = genPTxIn poolTxRef pdh x 100 y 100 lq 9223372036854775797 nft 1 5000000
    poolTxOut   = genPTxOut pdh x 110 y 90 lq 9223372036854775787 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose poolTxRef

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkOrderRedeemer 0 1 1

    result = eraseBoth $ evalWithArgs (wrapValidator PSwap.swapValidatorT) [cfgData, orderRedeemToData, cxtToData]

  result === Left ()

swapPoolIdentity :: Property
swapPoolIdentity = property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genSConfig x y nft 100 100 100 pkh 10 1
    orderTxIn     = genSTxIn orderTxRef dh x 10 5000000
    orderTxOut    = genSTxOut dh y 10 5000000 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 1
    poolTxIn    = genPTxIn poolTxRef pdh x 100 y 100 lq 9223372036854775797 nft 2 5000000
    poolTxOut   = genPTxOut pdh x 110 y 90 lq 9223372036854775787 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose orderTxRef

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkOrderRedeemer 0 1 1

    result = eraseBoth $ evalWithArgs (wrapValidator PSwap.swapValidatorT) [cfgData, orderRedeemToData, cxtToData]

  result === Left ()

swapIncorrectPoolInIx :: Property
swapIncorrectPoolInIx = property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genSConfig x y nft 100 100 100 pkh 10 1
    orderTxIn     = genSTxIn orderTxRef dh x 10 5000000
    orderTxOut    = genSTxOut dh y 10 5000000 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 1
    poolTxIn    = genPTxIn poolTxRef pdh x 100 y 100 lq 9223372036854775797 nft 1 5000000
    poolTxOut   = genPTxOut pdh x 110 y 90 lq 9223372036854775787 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose orderTxRef

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkOrderRedeemer 1 1 1

    result = eraseBoth $ evalWithArgs (wrapValidator PSwap.swapValidatorT) [cfgData, orderRedeemToData, cxtToData]

  result === Left ()

swapIncorrectOrderInIx :: Property
swapIncorrectOrderInIx = property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genSConfig x y nft 100 100 100 pkh 10 1
    orderTxIn     = genSTxIn orderTxRef dh x 10 5000000
    orderTxOut    = genSTxOut dh y 10 5000000 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 1
    poolTxIn    = genPTxIn poolTxRef pdh x 100 y 100 lq 9223372036854775797 nft 1 5000000
    poolTxOut   = genPTxOut pdh x 110 y 90 lq 9223372036854775787 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose orderTxRef

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkOrderRedeemer 0 0 1

    result = eraseBoth $ evalWithArgs (wrapValidator PSwap.swapValidatorT) [cfgData, orderRedeemToData, cxtToData]

  result === Left ()

swapIncorrectRewardOutIx :: Property
swapIncorrectRewardOutIx = property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genSConfig x y nft 100 100 100 pkh 10 1
    orderTxIn     = genSTxIn orderTxRef dh x 10 5000000
    orderTxOut    = genSTxOut dh y 10 5000000 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 1
    poolTxIn    = genPTxIn poolTxRef pdh x 100 y 100 lq 9223372036854775797 nft 1 5000000
    poolTxOut   = genPTxOut pdh x 110 y 90 lq 9223372036854775787 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose orderTxRef

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkOrderRedeemer 0 1 0

    result = eraseBoth $ evalWithArgs (wrapValidator PSwap.swapValidatorT) [cfgData, orderRedeemToData, cxtToData]

  result === Left ()