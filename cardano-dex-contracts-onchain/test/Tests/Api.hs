{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Tests.Api where

import qualified ErgoDex.PContracts.PDeposit as PDeposit
import ErgoDex.PValidators

import Eval
import Gen.Utils

import Hedgehog

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog as HH

import Gen.Models
import Gen.DepositGen
import Gen.PoolGen

import PlutusLedgerApi.V2

import Plutarch
import Plutarch.Prelude
import Plutarch.DataRepr
import Plutarch.Api.V2.Contexts
import Plutarch.Lift
import PExtra.Ada
import qualified PExtra.API as API
import Debug.Trace

import Plutarch.Api.V1 (PValue (..))

checkPValueLength = testGroup "pValueLengthTests"
  [ HH.testProperty "pValueLength_correct_token_value_same_policy_id" correctTokensQtySamePolicyId
  , HH.testProperty "pValueLength_correct_token_value_different_policy_id" correctTokensQtyDifferentPolicyId
  , HH.testProperty "pValueLength_incorrect_token_value_same_policy_id" incorrectTokensQtyDifferentPolicyId
  ]

-- wrapper around pValueLength
internalPValueLengthTest :: ClosedTerm (PValue _ _ :--> PInteger :--> PBool)
internalPValueLengthTest = plam $ \testValue testQty -> unTermCont $ do
    let realQty     = API.pValueLength # testValue
    pure $ pif (realQty #== testQty) (pcon PTrue) perror

correctTokensQtySamePolicyId :: Property
correctTokensQtySamePolicyId = property $ do
  let 
    (x, y, nft, lq) = genAssetClasses
    -- x, y, lq, nft have same policy
    value     = mkValues [mkValue x 10, mkValue y 10, mkValue lq 10, mkValue nft 10, mkValue genFakeToken 100, mkAdaValue 10] mempty

    correctTokenQty = 6

    result = eraseRight $ evalWithArgs (internalPValueLengthTest # (pconstant value) # (pconstant correctTokenQty)) []

  result === Right ()

correctTokensQtyDifferentPolicyId :: Property
correctTokensQtyDifferentPolicyId = property $ do
  (x, y, nft, lq) <- forAll genRandomAssetClasses
  let
    value     = mkValues [mkValue x 10, mkValue y 10, mkValue lq 10, mkValue nft 10, mkAdaValue 10] mempty
    correctTokenQty = 5
    result = eraseRight $ evalWithArgs (internalPValueLengthTest # (pconstant value) # (pconstant correctTokenQty)) []

  result === Right ()

incorrectTokensQtyDifferentPolicyId :: Property
incorrectTokensQtyDifferentPolicyId = property $ do
  (x, y, nft, _) <- forAll genRandomAssetClasses
  let
    value     = mkValues [mkValue x 10, mkValue y 10, mkValue nft 10, mkAdaValue 10] mempty
    incorrectTokenQty = 5
    result = eraseLeft $ evalWithArgs (internalPValueLengthTest # (pconstant value) # (pconstant incorrectTokenQty)) []

  result === Left ()