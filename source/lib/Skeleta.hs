{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Skeleta
    ( printData
    , getSearchSpaceSize
    , getIrreducibleSearchSpaceSize
    , codify
    , getStructure
    , toPosition) where

import           Data.List (foldl', sort)

type Code = [Int]

type Struct = [[Int]]

newtype Position = Position Int

printData :: IO ()
printData = print (codify $ Position 37) >> print (getStructure $ Position 37)

items :: Int
items = 4

toPosition :: Int -> Position
toPosition = Position

getOddNumbers :: Int -> [Int]
getOddNumbers n = [1, 3 .. (2 * n)]

getAccumulatedValues :: [Int] -> [Int]
getAccumulatedValues = foldl' go []
  where
    go acc n = let k = if null acc
                       then 1
                       else head acc
               in (n * k):acc

getSearchSpaceSize :: Int -> Int
getSearchSpaceSize n = foldl' (*) 1 $ getOddNumbers n

codify :: Position -> Code
codify (Position n) =
  let products = tail $ getAccumulatedValues $ getOddNumbers items
  in codify_ n products []

codify_ :: Int -> [Int] -> Code -> Code
codify_ _ [] representation = 0:representation
codify_ n products representation =
  let k = head products
      x = n `rem` k
      y = n `quot` k
  in codify_ x (tail products) (y:representation)

getStructure :: Position -> Struct
getStructure n = let a = [0, 2 .. (2 * items)]
                     b = [1, 3 .. (2 * items)]
                     representation = codify n
                     pairs = zipWith (\x y -> [x, y]) a b
                 in getStructure_ representation pairs []

getStructure_ :: Code -> Struct -> Struct -> Struct
getStructure_ [] _ structure = structure
getStructure_ representation pairs structure =
  let openItem = head representation
      [freeItem, closeItem] = head pairs
      newPair = [openItem, closeItem]
      newStructure = newPair:updateStructure (freeItem, openItem) structure
  in getStructure_ (tail representation) (tail pairs) newStructure

updateStructure :: (Int, Int) -> Struct -> Struct
updateStructure (free, open) = map go
  where
    go pair
      | open `elem` pair = sort $ free:filter (/= open) pair
      | otherwise = pair

getIrreducibleSearchSpaceSize :: Int -> Int
getIrreducibleSearchSpaceSize 1 = 1
getIrreducibleSearchSpaceSize 2 = 1
getIrreducibleSearchSpaceSize n =
  let elements = [1 .. (n - 1)]
      this = getIrreducibleSearchSpaceSize
      go k = (2 * k - 1) * this k * this (n - k)
  in sum $ map go elements
