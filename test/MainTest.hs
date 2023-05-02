import qualified SkeletaTest as ST
import           Test.Tasty

main :: IO ()
main = defaultMain
  $ testGroup
    "Tests"
    [ ST.unitTestsToGetIrreducibleCodeToStruct
    , ST.unitTestsToCalculateSearchSpaces
    , ST.unitTestsToGetStructuresWithinSmallSpaces
    , ST.unitTestsToAddStructuresWithinSmallSpaces
    , ST.unitTestsToCodifyIntegersWithinSmallSpaces
    , ST.unitTestsToGetIrreduciblePacksWithinSmallSpaces
    , ST.unitTestsToGetIrreducibleModelInSpaceWithinSmallSpaces]
