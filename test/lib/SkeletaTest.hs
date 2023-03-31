module SkeletaTest
    ( unitTestsToCalculateSearchSpaces
    , unitTestsToGetStructuresWithinSmallSpaces
    , unitTestsToAddStructuresWithinSmallSpaces
    , unitTestsToCodifyIntegersWithinSmallSpaces
    , unitTestsToGetIrreduciblePacksWithinSmallSpaces
    , unitTestsToGetIrreducibleModelInSpaceWithinSmallSpaces) where

import           Skeleta (codify, getIrreducibleSearchSpaceSize
                        , getIrreduciblePacks, getIrreducibleModelInSpace 
                        , getSearchSpaceSize, addIrreducibleStructures, getStructure, toPosition)
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
unitTestsToGetIrreduciblePacksWithinSmallSpaces = testGroup
  "Skeleta Unit Tests (getIrreduciblePacks)"
  [ testCase "For n = 3, it returns [1, 3]"
    $ assertEqual "==" [1, 3] (getIrreduciblePacks 3)
  , testCase "For n = 4, it returns [4, 3, 20]"
    $ assertEqual "==" [4, 3, 20] (getIrreduciblePacks 4)
  , testCase "For n = 5, it returns [27, 12, 20, 189]"
    $ assertEqual "==" [27, 12, 20, 189] (getIrreduciblePacks 5)]

unitTestsToGetIrreducibleModelInSpaceWithinSmallSpaces :: TestTree
unitTestsToGetIrreducibleModelInSpaceWithinSmallSpaces =
  let go n = getIrreducibleModelInSpace 3 $ toPosition n
  in testGroup
       "Skeleta Unit Tests (getIrreducibleModelInSpace)"
       [ testCase "For n = 3, it returns the 0-irreducible-structure"
         $ assertEqual "==" (0, (1, 0), (2, 0)) (go 0)
       , testCase "For n = 3, it returns the 1-irreducible-structure"
         $ assertEqual "==" (0, (2, 0), (1, 0)) (go 1)
       , testCase "For n = 3, it returns the 2-irreducible-structure"
         $ assertEqual "==" (1, (2, 0), (1, 0)) (go 2)
       , testCase "For n = 3, it returns the 3-irreducible-structure"
         $ assertEqual "==" (2, (2, 0), (1, 0)) (go 3)]

-- FIXME: Change to start with 0!
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
         $ assertEqual "==" [[1, 3], [2, 4]] (go (1, m1, m1))
       , testCase "It applys as f(1, m1, m2)=[[1, 5], [2, 4], [3, 6]]"
         $ assertEqual "==" [[1, 5], [2, 4], [3, 6]] (go (1, m1, m2))
       , testCase "It applys as f(1, m2, m1)=[[1, 4], [2, 6], [3, 5]]"
         $ assertEqual "==" [[1, 4], [2, 6], [3, 5]] (go (1, m2, m1))
       , testCase "It applys as f(2, m2, m1)=[[1, 4], [2, 5], [3, 6]]"
         $ assertEqual "==" [[1, 4], [2, 5], [3, 6]] (go (2, m2, m1))
       , testCase "It applys as f(3, m2, m1)=[[1, 3], [2, 5], [4, 6]]"
         $ assertEqual "==" [[1, 3], [2, 5], [4, 6]] (go (3, m2, m1))
       , testCase "It applys as f(5, m5, m1)=[[1, 3], [2, 5], [4, 7], [6, 8]]"
         $ assertEqual "==" [[1, 3], [2, 5], [4, 7], [6, 8]] (go (5, m5, m1))
         -- FIXME: It should return a failure (it's a reducible, instead of irreducible)
         -- , testCase "It applys as f(3, m2, m2)=[[1, 7], [2, 8], [3, 5], [4, 6]]"
         --    $ assertEqual "==" [[1, 7], [2, 8], [3, 5], [4, 6]] (go (2, m2, m2))
       , testCase
           "It applys as f(2, m2, m3)=[[1, 8], [2, 9], [3, 7], [4, 6], [5, 10]]"
         $ assertEqual
           "=="
           [[1, 8], [2, 9], [3, 7], [4, 6], [5, 10]]
           (go (2, m2, m3))
       , testCase
           "It applys as f(1, go (5, m5, m1), m4)=[[1, 8], [2, 5], [3, 14], [4, 6], [7, 10], [9, 12], [11, 13]]"
         $ assertEqual
           "=="
           [[1, 8], [2, 5], [3, 14], [4, 6], [7, 10], [9, 12], [11, 13]]
           (go (1, go (5, m5, m1), m4))]
