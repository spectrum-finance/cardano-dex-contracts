module Tests.Swap where

import qualified ErgoDex.PContracts.PPool as PPool
import qualified ErgoDex.PContracts.PSwap as PSwap
import ErgoDex.PValidators

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

checkSwap = testGroup "CheckSwap"
  [ HH.testProperty "correct_swap" successSwap
  , HH.testProperty "correct_swap_x_is_ada" successSwapWithXIsAda
  , HH.testProperty "correct_swap_y_is_ada" successSwapWithYIsAda
  , HH.testProperty "swap_invalid_ex_fee" invalidExFee
  , HH.testProperty "swap_invalid_fair_price" invalidFairPrice
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
  
successSwap :: Property
successSwap = property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genSConfig x y nft 995 500000 1 pkh 10 4
    orderTxIn     = genSTxIn orderTxRef dh x 10 3813762
    orderTxOut    = genSTxOut dh y 4 1813762 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 1 [] 0
    poolTxIn    = genPTxIn poolTxRef pdh x 10 y 10 lq 9223372036854775797 nft 1 1000000000
    poolTxOut   = genPTxOut pdh x 20 y 6 lq 9223372036854775787 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose orderTxRef

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkOrderRedeemer 0 1 1

    result = eraseRight $ evalWithArgs (wrapValidator PSwap.swapValidatorT) [cfgData, orderRedeemToData, cxtToData]

  result === Right ()

successSwapWithXIsAda :: Property
successSwapWithXIsAda = property $ do
  let (_, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  orderTxRef      <- forAll genTxOutRef
  let
    x             = mkAdaAssetClass 
    (cfgData, dh) = genSConfig x y nft 100 100 100 pkh 10 1
    orderTxIn     = genSTxIn orderTxRef dh x 5010 0
    orderTxOut    = genSTxOut dh y 10 5000 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 1 [] 0
    poolTxIn    = genPTxIn poolTxRef pdh x 5000 y 5000 lq 9223372036854775797 nft 1 0
    poolTxOut   = genPTxOut pdh x 5010 y 4990 lq 9223372036854775787 nft 1 0 
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose orderTxRef

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkOrderRedeemer 0 1 1

    result = eraseRight $ evalWithArgs (wrapValidator PSwap.swapValidatorT) [cfgData, orderRedeemToData, cxtToData]

  result === Right ()

successSwapWithYIsAda :: Property
successSwapWithYIsAda = property $ do
  let (x, _, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  orderTxRef      <- forAll genTxOutRef
  let
    y             = mkAdaAssetClass 
    (cfgData, dh) = genSConfig x y nft 995 4160772239327619 100000000000000 pkh 49405 48068
    orderTxIn     = genSTxIn orderTxRef dh x 49405 3489838
    orderTxOut    = genSTxOut dh y 0 1479634 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 1 [] 0
    poolTxIn    = genPTxIn poolTxRef pdh x 9940655 y 0 lq 9223372036844775807 nft 1 10060000
    poolTxOut   = genPTxOut pdh x 9990060 y 0 lq 9223372036854775787 nft 1 10010497
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose orderTxRef

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkOrderRedeemer 0 1 1

    result = eraseRight $ evalWithArgs (wrapValidator PSwap.swapValidatorT) [cfgData, orderRedeemToData, cxtToData]

  result === Right ()

invalidFairPrice :: Property
invalidFairPrice = property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genSConfig x y nft 1000 100 1 pkh 10 1
    orderTxIn     = genSTxIn orderTxRef dh x 10 5000000
    orderTxOut    = genSTxOut dh y 10 500000 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 1 [] 0
    poolTxIn    = genPTxIn poolTxRef pdh x 10 y 100 lq 9223372036854775797 nft 1 5000000
    poolTxOut   = genPTxOut pdh x 110 y 90 lq 9223372036854775787 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose orderTxRef

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkOrderRedeemer 0 1 1

    result = eraseBoth $ evalWithArgs (wrapValidator PSwap.swapValidatorT) [cfgData, orderRedeemToData, cxtToData]

  result === Left ()

invalidExFee :: Property
invalidExFee = property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genSConfig x y nft 100 100 100 pkh 10 1
    orderTxIn     = genSTxIn orderTxRef dh x 10 5000000
    orderTxOut    = genSTxOut dh y 10 500000 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 1 [] 0
    poolTxIn    = genPTxIn poolTxRef pdh x 100 y 100 lq 9223372036854775797 nft 1 5000000
    poolTxOut   = genPTxOut pdh x 110 y 90 lq 9223372036854775787 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose orderTxRef

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkOrderRedeemer 0 1 1

    result = eraseBoth $ evalWithArgs (wrapValidator PSwap.swapValidatorT) [cfgData, orderRedeemToData, cxtToData]

  result === Left ()

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
    (pcfg, pdh) = genPConfig x y nft lq 1 [] 0
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
    (pcfg, pdh) = genPConfig x y nft lq 1 [] 0
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
    (pcfg, pdh) = genPConfig x y nft lq 1 [] 0
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
    (pcfg, pdh) = genPConfig x y nft lq 1 [] 0
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
    (pcfg, pdh) = genPConfig x y nft lq 1 [] 0
    poolTxIn    = genPTxIn poolTxRef pdh x 100 y 100 lq 9223372036854775797 nft 1 5000000
    poolTxOut   = genPTxOut pdh x 110 y 90 lq 9223372036854775787 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose orderTxRef

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkOrderRedeemer 0 1 0

    result = eraseBoth $ evalWithArgs (wrapValidator PSwap.swapValidatorT) [cfgData, orderRedeemToData, cxtToData]

  result === Left ()