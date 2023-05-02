module SkeletaTest
    ( unitTestsToCalculateSearchSpaces
    , unitTestsToGetIrreducibleCodeToStruct
    , unitTestsToGetStructuresWithinSmallSpaces
    , unitTestsToAddStructuresWithinSmallSpaces
    , unitTestsToCodifyIntegersWithinSmallSpaces
    , unitTestsToGetIrreduciblePacksWithinSmallSpaces
    , unitTestsToGetIrreducibleModelInSpaceWithinSmallSpaces) where

import           Skeleta (codify, getIrreducibleSearchSpaceSize
                        , getIrreducibleCodeToStruct, getIrreduciblePacks
                        , getIrreducibleModelInSpace, getSearchSpaceSize
                        , addIrreducibleStructures, getStructure, toPosition
                        , totalValue)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Data.List (sort)

unitTestsToCalculateSearchSpaces :: TestTree
unitTestsToCalculateSearchSpaces = testGroup
  "Skeleta Unit Tests (getSearchSpaceSize, getIrreducibleSearchSpaceSize)"
  [ testCase "For n = 5, it returns 248 irreducible matchings"
    $ assertEqual "==" 248 (getIrreducibleSearchSpaceSize 5)
  , testCase "For n = 4, it returns 27 irreducible matchings"
    $ assertEqual "==" 27 (getIrreducibleSearchSpaceSize 4)
  , testCase "For n = 3, it returns 4 irreducible matchings"
    $ assertEqual "==" 4 (getIrreducibleSearchSpaceSize 3)
  , testCase "For n = 6, it returns 2830 irreducible matchings"
    $ assertEqual "==" 2830 (getIrreducibleSearchSpaceSize 6)
  , testCase "For n = 2, it returns 1 irreducible matching"
    $ assertEqual "==" 1 (getIrreducibleSearchSpaceSize 2)
  , testCase "For n = 4, it returns 105 complete matchings"
    $ assertEqual "==" 105 (getSearchSpaceSize 4)
  , testCase "For n = 3, it returns 15 complete matchings"
    $ assertEqual "==" 15 (getSearchSpaceSize 3)]

unitTestsToCodifyIntegersWithinSmallSpaces :: TestTree
unitTestsToCodifyIntegersWithinSmallSpaces =
  let go = codify . toPosition
  in testGroup
       "Skeleta Unit Tests (codify)"
       [ testCase "For n = 4, it returns the 0 codified"
         $ assertEqual "==" [0, 0, 0, 0] (go 0)
       , testCase "For n = 4, it returns the 29 codified"
         $ assertEqual "==" [0, 2, 4, 1] (go 29)
       , testCase "For n = 4, it returns the 37 codified"
         $ assertEqual "==" [0, 1, 2, 2] (go 37)
       , testCase "For n = 4, it returns the 104 codified"
         $ assertEqual "==" [0, 2, 4, 6] (go 104)]

unitTestsToGetStructuresWithinSmallSpaces :: TestTree
unitTestsToGetStructuresWithinSmallSpaces =
  let go = sort . getStructure . toPosition
  in testGroup
       "Skeleta Unit Tests (getStructure)"
       [ testCase "For n = 4, it returns the 0-structure"
         $ assertEqual "==" [[0, 7], [1, 2], [3, 4], [5, 6]] (go 0)
       , testCase "For n = 4, it returns the 29-structure"
         $ assertEqual "==" [[0, 6], [1, 7], [2, 3], [4, 5]] (go 29)
       , testCase "For n = 4, it returns the 37-structure"
         $ assertEqual "==" [[0, 4], [1, 3], [2, 7], [5, 6]] (go 37)
       , testCase "For n = 4, it returns the 104-structure"
         $ assertEqual "==" [[0, 1], [2, 3], [4, 5], [6, 7]] (go 104)]

unitTestsToGetIrreduciblePacksWithinSmallSpaces :: TestTree
unitTestsToGetIrreduciblePacksWithinSmallSpaces =
  let go k = map totalValue $ getIrreduciblePacks k
  in testGroup
       "Skeleta Unit Tests (getIrreduciblePacks)"
       [ testCase "For n = 3, it returns [1, 3]"
         $ assertEqual "==" [1, 3] (go 3)
       , testCase "For n = 4, it returns [4, 3, 20]"
         $ assertEqual "==" [4, 3, 20] (go 4)
       , testCase "For n = 5, it returns [27, 12, 20, 189]"
         $ assertEqual "==" [27, 12, 20, 189] (go 5)
       , testCase "For n = 6, it returns [248, 81, 80, 189, 2232]"
         $ assertEqual "==" [248, 81, 80, 189, 2232] (go 6)
       , testCase "For n = 7, it returns [2830, 744, 540, 756, 2232, 31130]"
         $ assertEqual "==" [2830, 744, 540, 756, 2232, 31130] (go 7)]

unitTestsToGetIrreducibleModelInSpaceWithinSmallSpaces :: TestTree
unitTestsToGetIrreducibleModelInSpaceWithinSmallSpaces =
  let go n p = getIrreducibleModelInSpace n $ toPosition p
      go3 = go 3
      go5 = go 5
      go6 = go 6
      go7 = go 7
  in testGroup
       "Skeleta Unit Tests (getIrreducibleModelInSpace)"
       [ testCase "For n = 3, it returns the 0-irreducible-structure"
         $ assertEqual "==" (0, (1, 0), (2, 0)) (go3 0)
       , testCase "For n = 3, it returns the 1-irreducible-structure"
         $ assertEqual "==" (0, (2, 0), (1, 0)) (go3 1)
       , testCase "For n = 3, it returns the 2-irreducible-structure"
         $ assertEqual "==" (1, (2, 0), (1, 0)) (go3 2)
       , testCase "For n = 3, it returns the 3-irreducible-structure"
         $ assertEqual "==" (2, (2, 0), (1, 0)) (go3 3)
       , testCase "For n = 5, it returns the 40-irreducible-structure"
         $ assertEqual "==" (0, (3, 1), (2, 0)) (go5 40)
       , testCase "For n = 6, it returns the 383-irreducible-structure"
         $ assertEqual "==" (3, (3, 1), (3, 2)) (go6 383)
       , testCase "For n = 7, it returns the 3972-irreducible-structure"
         $ assertEqual "==" (3, (3, 2), (4, 20)) (go7 3972)]

unitTestsToGetIrreducibleCodeToStruct :: TestTree
unitTestsToGetIrreducibleCodeToStruct = testGroup
  "Skeleta Unit Tests (getIrreducibleCodeToStruct)"
  [ testCase "It returns [[1, 2]] given (0, (0, 0), (0, 0))"
    $ assertEqual
      "=="
      [[1, 2]]
      (getIrreducibleCodeToStruct (0, (0, 0), (0, 0)))
  , testCase "It returns [[1, 3], [2, 4]] given (0, (1, 0), (1, 0))"
    $ assertEqual
      "=="
      [[1, 3], [2, 4]]
      (getIrreducibleCodeToStruct (0, (1, 0), (1, 0)))]

unitTestsToAddStructuresWithinSmallSpaces :: TestTree
unitTestsToAddStructuresWithinSmallSpaces =
  let m1 = [[1, 2]]
      m2 = [[1, 3], [2, 4]]
      m3 = [[1, 5], [2, 4], [3, 6]]
      m4 = [[1, 4], [2, 6], [3, 5]]
      m5 = [[1, 3], [2, 5], [4, 6]]
      go = sort . addIrreducibleStructures
  in testGroup
       "Skeleta Unit Tests (addIrreducibleStructures)"
       [ testCase "It applys as f(1, m1, m1)=[[1, 3], [2, 4]]"
         $ assertEqual "==" [[1, 3], [2, 4]] (go (0, m1, m1))
       , testCase "It applys as f(1, m1, m2)=[[1, 5], [2, 4], [3, 6]]"
         $ assertEqual "==" [[1, 5], [2, 4], [3, 6]] (go (0, m1, m2))
       , testCase "It applys as f(1, m2, m1)=[[1, 4], [2, 6], [3, 5]]"
         $ assertEqual "==" [[1, 4], [2, 6], [3, 5]] (go (0, m2, m1))
       , testCase "It applys as f(2, m2, m1)=[[1, 4], [2, 5], [3, 6]]"
         $ assertEqual "==" [[1, 4], [2, 5], [3, 6]] (go (1, m2, m1))
       , testCase "It applys as f(3, m2, m1)=[[1, 3], [2, 5], [4, 6]]"
         $ assertEqual "==" [[1, 3], [2, 5], [4, 6]] (go (2, m2, m1))
       , testCase "It applys as f(5, m5, m1)=[[1, 3], [2, 5], [4, 7], [6, 8]]"
         $ assertEqual "==" [[1, 3], [2, 5], [4, 7], [6, 8]] (go (4, m5, m1))
         -- FIXME: It should return a failure (it's a reducible, instead of a irreducible)
         -- , testCase "It applys as f(3, m2, m2)=[[1, 7], [2, 8], [3, 5], [4, 6]]"
         --    $ assertEqual "==" [[1, 7], [2, 8], [3, 5], [4, 6]] (go (2, m2, m2))
       , testCase
           "It applys as f(2, m2, m3)=[[1, 8], [2, 9], [3, 7], [4, 6], [5, 10]]"
         $ assertEqual
           "=="
           [[1, 8], [2, 9], [3, 7], [4, 6], [5, 10]]
           (go (1, m2, m3))
       , testCase
           "It applys as f(1, go (5, m5, m1), m4)=[[1, 8], [2, 5], [3, 14], [4, 6], [7, 10], [9, 12], [11, 13]]"
         $ assertEqual
           "=="
           [[1, 8], [2, 5], [3, 14], [4, 6], [7, 10], [9, 12], [11, 13]]
           (go (0, go (4, m5, m1), m4))]
