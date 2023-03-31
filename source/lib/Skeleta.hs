{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Skeleta
    ( codify
    , printData
    , toPosition
    , getStructure
    , getSearchSpaceSize
    , getIrreduciblePacks
    , addIrreducibleStructures
    , getIrreducibleModelInSpace
    , getIrreducibleSearchSpaceSize) where

import           Data.List (foldl', sort)

type Code = [Int]

type Struct = [[Int]]

type Arc = (Int, Int)

type Irreducible = (Int, Arc, Arc)

newtype Position = Position Int

printData :: IO ()
printData = print (codify $ Position 37) >> print (getStructure $ Position 37)

{-- 'n' value related with the space to search --}
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

getIrreduciblePacks :: Int -> [Int]
getIrreduciblePacks 1 = [1]
getIrreduciblePacks 2 = [1]
getIrreduciblePacks n = let elements = [1 .. (n - 1)]
                            calc = sum . getIrreduciblePacks
                            go k = (2 * k - 1) * calc k * calc (n - k)
                        in map go elements

getIrreducibleSearchSpaceSize :: Int -> Int
getIrreducibleSearchSpaceSize = sum . getIrreduciblePacks

calculatePosition :: [Int] -> Int -> Int
calculatePosition packs m =
  let t = tail packs
      h = head packs
  in if m < h
     then m
     else calculatePosition t (m - h)

getTerm :: [Int] -> (Int, Int) -> Int
getTerm [] (_, i) = i
getTerm terms (m, i) =
  let t = tail terms
      h = head terms
  in if m < h
     then h
     else getTerm t (m - h, i)

getIrreducibleModelInSpace :: Int -> Position -> Irreducible
getIrreducibleModelInSpace 2 _ = (0, (1, 0), (1, 0))
getIrreducibleModelInSpace n (Position m) =
  let packs = getIrreduciblePacks n
      i = calculatePosition packs m
      k = getTerm [1 .. (n - 1)] (m, i)
  in (i, (k, 0), (n - k, 0))

-- FIXME: What happen when k > 2|x| - 2, if k start at 0?
addIrreducibleStructures :: (Int, Struct, Struct) -> Struct
addIrreducibleStructures (_, [], []) = []
addIrreducibleStructures (_, [], y) = y
addIrreducibleStructures (_, x, []) = x
addIrreducibleStructures (k, x, y) =
  let sizeX = 2 * length x
      sizeY = 2 * length y - 1
      fullSize = sizeX + sizeY + 1
      goX [a, b]
        | a > k && b > k = [a + sizeY, b + sizeY]
        | a > k = sort [a + sizeY, b]
        | b > k = sort [a, b + sizeY]
        | otherwise = [a, b]
      goY [a, b]
        | b + sizeX == fullSize = sort [a + k, b + sizeX]
        | otherwise = [a + k, b + k]
  in map goX x ++ map goY y
