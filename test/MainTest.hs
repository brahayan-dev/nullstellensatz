import qualified SkeletaTest as ST
import           Test.Tasty

main :: IO ()
main = defaultMain
  $ testGroup
    "Tests"
    [ ST.unitTestsToCalculateSearchSpaces
    , ST.unitTestsToGetIrreducibleCodeToStruct
    , ST.unitTestsToGetStructuresWithinSmallSpaces
    , ST.unitTestsToAddStructuresWithinSmallSpaces
    , ST.unitTestsToCodifyIntegersWithinSmallSpaces
    , ST.unitTestsToGetIrreduciblePacksWithinSmallSpaces
    , ST.unitTestsToGetIrreducibleModelInSpaceWithinSmallSpaces]
