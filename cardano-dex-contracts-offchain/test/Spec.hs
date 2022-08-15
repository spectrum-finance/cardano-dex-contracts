module Main(main) where

import Tests.Contracts 

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  defaultMain tests

tests = testGroup "Contracts"
  [ checkContractsRecovering
  ]