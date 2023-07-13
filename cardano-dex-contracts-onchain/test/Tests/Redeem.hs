module Tests.Redeem where

import qualified ErgoDex.PContracts.PPool as PPool
import qualified ErgoDex.PContracts.PRedeem as PRedeem
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
import Gen.RedeemGen

checkRedeem = testGroup "CheckRedeem"
  [ HH.testProperty "correct_redeem" successRedeem
  , HH.testProperty "correct_redeem_x_is_ada" successRedeemXAda
  , HH.testProperty "correct_redeem_y_is_ada" successRedeemYAda
  ]

checkRedeemRedeemer = testGroup "RedeemRedeemer"
  [ HH.testProperty "fail_if_poolInIx_is_incorrect" redeemIncorrectPoolInIx
  , HH.testProperty "fail_if_orderInIx_is_incorrect" redeemIncorrectOrderInIx
  , HH.testProperty "fail_if_rewardOutIx_is_incorrect" redeemIncorrectRewardOutIx
  ]

checkRedeemIdentity = testGroup "checkRedeemIdentity"
  [ HH.testProperty "fail_if_selfIdentity_is_incorrect" redeemSelfIdentity
  , HH.testProperty "fail_if_poolIdentity_is_incorrect" redeemPoolIdentity
  ]

checkRedeemIsFair = testGroup "checkRedeemIdentity"
  [ HH.testProperty "fail_if_fair_fee_is_incorrect" redeemFairFee
  , HH.testProperty "fail_if_fair_share_x_is_incorrect" redeemFairShareX
  , HH.testProperty "fail_if_fair_share_y_is_incorrect" redeemFairShareY
  ]
  
successRedeem :: Property
successRedeem = property $ do
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
    purpose = mkPurpose orderTxRef

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkOrderRedeemer 0 1 1

    result = eraseRight $ evalWithArgs (wrapValidator PRedeem.redeemValidatorT) [cfgData, orderRedeemToData, cxtToData]

  result === Right ()

successRedeemXAda :: Property
successRedeemXAda = property $ do
  let (_, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  orderTxRef      <- forAll genTxOutRef
  let
    x             = mkAdaAssetClass
    (cfgData, dh) = genRConfig x y lq nft 100 pkh
    orderTxIn     = genRTxIn orderTxRef dh lq 10000 1100
    orderTxOut    = genRTxOut dh x 11000 y 11000 0 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 1 [] 0
    poolTxIn    = genPTxIn poolTxRef pdh x 20000 y 20000 lq 9223372036854755797 nft 1 0
    poolTxOut   = genPTxOut pdh x 10000 y 10000 lq 9223372036854765797 nft 1 0
          
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose orderTxRef

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkOrderRedeemer 0 1 1

    result = eraseRight $ evalWithArgs (wrapValidator PRedeem.redeemValidatorT) [cfgData, orderRedeemToData, cxtToData]

  result === Right ()

successRedeemYAda :: Property
successRedeemYAda = property $ do
  let (x, _, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  orderTxRef      <- forAll genTxOutRef
  let
    y             = mkAdaAssetClass
    (cfgData, dh) = genRConfig x y lq nft 100 pkh
    orderTxIn     = genRTxIn orderTxRef dh lq 10000 1100
    orderTxOut    = genRTxOut dh x 11000 y 11000 0 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 1 [] 0
    poolTxIn    = genPTxIn poolTxRef pdh x 20000 y 20000 lq 9223372036854755797 nft 1 0
    poolTxOut   = genPTxOut pdh x 10000 y 10000 lq 9223372036854765797 nft 1 0
          
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose orderTxRef

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkOrderRedeemer 0 1 1

    result = eraseRight $ evalWithArgs (wrapValidator PRedeem.redeemValidatorT) [cfgData, orderRedeemToData, cxtToData]

  result === Right ()

redeemIncorrectPoolInIx:: Property
redeemIncorrectPoolInIx = property $ do
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
    purpose = mkPurpose orderTxRef

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkOrderRedeemer 1 1 1

    result = eraseBoth $ evalWithArgs (wrapValidator PRedeem.redeemValidatorT) [cfgData, orderRedeemToData, cxtToData]

  result === Left ()

redeemIncorrectOrderInIx :: Property
redeemIncorrectOrderInIx = property $ do
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
    purpose = mkPurpose orderTxRef

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkOrderRedeemer 0 0 1

    result = eraseBoth $ evalWithArgs (wrapValidator PRedeem.redeemValidatorT) [cfgData, orderRedeemToData, cxtToData]

  result === Left ()

redeemIncorrectRewardOutIx :: Property
redeemIncorrectRewardOutIx = property $ do
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
    purpose = mkPurpose orderTxRef

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkOrderRedeemer 0 1 0

    result = eraseBoth $ evalWithArgs (wrapValidator PRedeem.redeemValidatorT) [cfgData, orderRedeemToData, cxtToData]

  result === Left ()

redeemSelfIdentity :: Property
redeemSelfIdentity = property $ do
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

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkOrderRedeemer 0 1 1

    result = eraseBoth $ evalWithArgs (wrapValidator PRedeem.redeemValidatorT) [cfgData, orderRedeemToData, cxtToData]

  result === Left ()

redeemPoolIdentity :: Property
redeemPoolIdentity = property $ do
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
    poolTxIn    = genPTxIn poolTxRef pdh x 20 y 20 lq 9223372036854775787 nft 2 5000000
    poolTxOut   = genPTxOut pdh x 10 y 10 lq 9223372036854775797 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose orderTxRef

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkOrderRedeemer 0 1 1

    result = eraseBoth $ evalWithArgs (wrapValidator PRedeem.redeemValidatorT) [cfgData, orderRedeemToData, cxtToData]

  result === Left ()

redeemFairFee :: Property
redeemFairFee = property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genRConfig x y lq nft 100 pkh
    orderTxIn     = genRTxIn orderTxRef dh lq 10 20000
    orderTxOut    = genRTxOut dh x 10 y 10 10000 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 1 [] 0
    poolTxIn    = genPTxIn poolTxRef pdh x 20 y 20 lq 9223372036854775787 nft 1 5000000
    poolTxOut   = genPTxOut pdh x 10 y 10 lq 9223372036854775797 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose orderTxRef

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkOrderRedeemer 0 1 1

    result = eraseBoth $ evalWithArgs (wrapValidator PRedeem.redeemValidatorT) [cfgData, orderRedeemToData, cxtToData]

  result === Left ()

redeemFairShareX :: Property
redeemFairShareX = property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genRConfig x y lq nft 100 pkh
    orderTxIn     = genRTxIn orderTxRef dh lq 10 5000000
    orderTxOut    = genRTxOut dh x 9 y 10 5000000 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 1 [] 0
    poolTxIn    = genPTxIn poolTxRef pdh x 20 y 20 lq 9223372036854775787 nft 1 5000000
    poolTxOut   = genPTxOut pdh x 10 y 10 lq 9223372036854775797 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose orderTxRef

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkOrderRedeemer 0 1 1

    result = eraseBoth $ evalWithArgs (wrapValidator PRedeem.redeemValidatorT) [cfgData, orderRedeemToData, cxtToData]

  result === Left ()

redeemFairShareY :: Property
redeemFairShareY = property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  orderTxRef      <- forAll genTxOutRef
  let
    (cfgData, dh) = genRConfig x y lq nft 100 pkh
    orderTxIn     = genRTxIn orderTxRef dh lq 10 5000000
    orderTxOut    = genRTxOut dh x 10 y 9 5000000 pkh
  
  poolTxRef <- forAll genTxOutRef
  let
    (pcfg, pdh) = genPConfig x y nft lq 1 [] 0
    poolTxIn    = genPTxIn poolTxRef pdh x 20 y 20 lq 9223372036854775787 nft 1 5000000
    poolTxOut   = genPTxOut pdh x 10 y 10 lq 9223372036854775797 nft 1 3000000
  
  let
    txInfo  = mkTxInfo poolTxIn orderTxIn poolTxOut orderTxOut
    purpose = mkPurpose orderTxRef

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkOrderRedeemer 0 1 1

    result = eraseBoth $ evalWithArgs (wrapValidator PRedeem.redeemValidatorT) [cfgData, orderRedeemToData, cxtToData]

  result === Left ()