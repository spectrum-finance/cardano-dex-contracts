module Main(main) where

import Tests.Deposit 
import Tests.Pool 

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests = testGroup "Contracts"
  [  
  --   checkDeposit
  -- , checkDepositRedeemer
  -- , checkDepositIdentity
  -- , checkDepositLq
  -- , checkDepositTokenReward

     checkPoolDeposit
   , checkPoolDepositRedeemer
  ]

-- tests = 
--   TestList 
--     [ 

--     -- Pool deposit --
--     , TestLabel "Check pool deposit correctness." (TestCase $ runSuccessDepositePool >>= (assertBool "Pool(deposit) contract should return true if input data is correct."))
--     , TestLabel "Check pool deposit redeemer." (TestCase $ (runFailurePoolDepositIncorrectRedeem 0) >>= (\res -> assertBool "If pool index is incorrect the contract should fail." (not res)))
--     , TestLabel "Check pool deposit redeemer." (TestCase $ (runFailurePoolDepositIncorrectRedeem 1) >>= (\res -> assertBool "If pool action is incorrect the contract should fail." (not res)))
--     , TestLabel "Check pool out value." (TestCase $ (runFailurePoolDepositIncorrectValue 0) >>= (\res -> assertBool "If X token in pool out value is incorrect the test should fail." (not res))) -- fails
--     , TestLabel "Check pool out value." (TestCase $ (runFailurePoolDepositIncorrectValue 1) >>= (\res -> assertBool "If Y token in pool out value is incorrect the test should fail." (not res))) -- fails
--     , TestLabel "Check pool out value." (TestCase $ (runFailurePoolDepositIncorrectValue 2) >>= (\res -> assertBool "If Nft token in pool out value is incorrect the test should fail." (not res))) -- fails
--     , TestLabel "Check pool out value." (TestCase $ (runFailurePoolDepositIncorrectValue 3) >>= (\res -> assertBool "If Lq token in pool out value is incorrect the test should fail." (not res)))
    
--     ]