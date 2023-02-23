import Test.Tasty

import qualified SkeletaTest as ST


main :: IO ()
main = defaultMain $ testGroup "Tests" [ST.unitTests]

