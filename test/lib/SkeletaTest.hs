module SkeletaTest
    ( unitTestsToGetStructuresWithinSmallSpaces
    , unitTestsToCodifyIntegersWithinSmallSpaces
    , unitTestsToCalculateSearchSpaces) where

import           Skeleta (codify, getIrreducibleSearchSpaceSize
                        , getSearchSpaceSize, getStructure, toPosition)
import           Test.Tasty
import           Test.Tasty.HUnit

unitTestsToCalculateSearchSpaces :: TestTree
unitTestsToCalculateSearchSpaces = testGroup
  "Skeleta Unit Tests (getSearchSpaceSize, getIrreducibleSearchSpaceSize)"
  [ testCase "For n = 5, it returns 248 irreducible matchings"
    $ assertEqual "==" 248 (getIrreducibleSearchSpaceSize 5)
  , testCase "For n = 4, it returns 27 irreducible matchings"
    $ assertEqual "==" 27 (getIrreducibleSearchSpaceSize 4)
  , testCase "For n = 3, it returns 4 irreducible matchings"
    $ assertEqual "==" 4 (getIrreducibleSearchSpaceSize 3)
  , testCase "For n = 2, it returns 1 irreducible matching"
    $ assertEqual "==" 1 (getIrreducibleSearchSpaceSize 2)
  , testCase "For n = 4, it returns 105 complete matchings"
    $ assertEqual "==" 105 (getSearchSpaceSize 4)
  , testCase "For n = 3, it returns 15 complete matchings"
    $ assertEqual "==" 15 (getSearchSpaceSize 3)]

unitTestsToCodifyIntegersWithinSmallSpaces :: TestTree
unitTestsToCodifyIntegersWithinSmallSpaces = testGroup
  "Skeleta Unit Tests (codify)"
  [ testCase "For n = 4, it returns the 0 codified"
    $ assertEqual "==" [0, 0, 0, 0] (codify $ toPosition 0)
  , testCase "For n = 4, it returns the 29 codified"
    $ assertEqual "==" [0, 2, 4, 1] (codify $ toPosition 29)
  , testCase "For n = 4, it returns the 37 codified"
    $ assertEqual "==" [0, 1, 2, 2] (codify $ toPosition 37)
  , testCase "For n = 4, it returns the 104 codified"
    $ assertEqual "==" [0, 2, 4, 6] (codify $ toPosition 104)]

unitTestsToGetStructuresWithinSmallSpaces :: TestTree
unitTestsToGetStructuresWithinSmallSpaces = testGroup
  "Skeleta Unit Tests (getStructure)"
  [ testCase "For n = 4, it returns the 0-structure"
    $ assertEqual
      "=="
      [[0, 7], [1, 2], [3, 4], [5, 6]]
      (getStructure $ toPosition 0)
  , testCase "For n = 4, it returns the 29-structure"
    $ assertEqual
      "=="
      [[0, 6], [1, 7], [2, 3], [4, 5]]
      (getStructure $ toPosition 29)
  , testCase "For n = 4, it returns the 37-structure"
    $ assertEqual
      "=="
      [[0, 4], [1, 3], [2, 7], [5, 6]]
      (getStructure $ toPosition 37)
  , testCase "For n = 4, it returns the 104-structure"
    $ assertEqual
      "=="
      [[0, 1], [2, 3], [4, 5], [6, 7]]
      (getStructure $ toPosition 104)]
