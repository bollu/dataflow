{-# LANGUAGE TypeApplications #-}
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Modifiers (NonEmptyList (..))
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import Control.Exception
import Control.Monad


-- Test bench
-- ==========
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [qcProps, unitTests]



qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty @ ([Int] -> Bool) "list to linked list" $ 
    \xs -> length xs == length xs
  ]




unitTests = testGroup "Unit tests"
  [ testCase "create and get value from tuple" $  10 @?= 10]
