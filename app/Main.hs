{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Data.List (foldl')

type Code = [Int]

type Struct = [[Int]]

newtype Position = Position Int

main :: IO ()
main =
  print (getSearchSpaceSize items)
    >> print (codify $ Position 63513)
    >> print (getStructure $ Position 13513)

items :: Int
items = 2 * 7

getOddNumbers :: Int -> [Int]
getOddNumbers n = [1, 3 .. n]

getAccumulatedValues :: [Int] -> [Int]
getAccumulatedValues =
  foldl' go []
  where
    go acc n =
      let k = if null acc then 1 else head acc
       in (n * k) : acc

getSearchSpaceSize :: Int -> Int
getSearchSpaceSize n = foldl' (*) 1 $ getOddNumbers n

codify :: Position -> Code
codify (Position n) =
  let products =
        tail $
          getAccumulatedValues $
            getOddNumbers items
   in codify_ n products []

codify_ :: Int -> [Int] -> Code -> Code
codify_ n products representation =
  let k = head products
      x = n `rem` k
      y = n `quot` k
   in if null products
        then 0 : representation
        else codify_ x (tail products) (y : representation)

getStructure :: Position -> Struct
getStructure n =
  let a = [0, 2 .. items]
      b = [1, 3 .. items]
      representation = codify n
      pairs = zipWith (\x y -> [x, y]) a b
   in getStructure_ representation pairs []

getStructure_ :: Code -> Struct -> Struct -> Struct
getStructure_ rs ps s =
  let openItem = head rs
      [freeItem, closeItem] = head ps
      newPair = [openItem, closeItem]
      structure = newPair : updateStructure (freeItem, openItem) s
   in if null rs
        then s
        else getStructure_ (tail rs) (tail ps) structure

updateStructure :: (Int, Int) -> Struct -> Struct
updateStructure (free, open) = map go
  where
    go pair =
      if open `elem` pair
        then free : filter (/= open) pair
        else pair
