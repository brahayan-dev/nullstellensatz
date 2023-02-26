module SkeletaTest
  ( unitTestsToGetStructuresWithinSmallSpaces,
    unitTestsToCodifyIntegersWithinSmallSpaces,
  )
where

import Skeleta (getStructure, codify, toPosition)
import Test.Tasty
import Test.Tasty.HUnit

unitTestsToCodifyIntegersWithinSmallSpaces :: TestTree
unitTestsToCodifyIntegersWithinSmallSpaces =
  testGroup
    "Skeleta Unit Tests (codify)"
    [ testCase "For n = 4, returns the 0 codified" $
        assertEqual "==" [0, 0, 0, 0] (codify $ toPosition 0),
      testCase "For n = 4, returns the 29 codified" $
        assertEqual "==" [0, 2, 4, 1] (codify $ toPosition 29),
      testCase "For n = 4, returns the 37 codified" $
        assertEqual "==" [0, 1, 2, 2] (codify $ toPosition 37),
      testCase "For n = 4, returns the 104 codified" $
        assertEqual "==" [0, 2, 4, 6] (codify $ toPosition 104)
    ]

unitTestsToGetStructuresWithinSmallSpaces :: TestTree
unitTestsToGetStructuresWithinSmallSpaces =
  testGroup
    "Skeleta Unit Tests (getStructure)"
    [ testCase "For n = 4, returns the 0-structure" $
        assertEqual "==" [[0, 7], [1, 2], [3, 4], [5, 6]] (getStructure $ toPosition 0),
      testCase "For n = 4, returns the 29-structure" $
        assertEqual "==" [[0, 6], [1, 7], [2, 3], [4, 5]] (getStructure $ toPosition 29),
      testCase "For n = 4, returns the 37-structure" $
        assertEqual "==" [[0, 4], [1, 3], [2, 7], [5, 6]] (getStructure $ toPosition 37),
      testCase "For n = 4, returns the 104-structure" $
        assertEqual "==" [[0, 1], [2, 3], [4, 5], [6, 7]] (getStructure $ toPosition 104)
    ]
