import qualified SkeletaTest as ST
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [ ST.unitTestsToCodifyIntegersWithinSmallSpaces,
        ST.unitTestsToGetStructuresWithinSmallSpaces
      ]
