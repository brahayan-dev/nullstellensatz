{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Skeleta
    ( codify
    , printData
    , totalValue
    , toPosition
    , getStructure
    , getSearchSpaceSize
    , getIrreduciblePacks
    , addIrreducibleStructures
    , getIrreducibleModelInSpace
    , getIrreducibleCodeToStruct
    , getIrreducibleSearchSpaceSize) where

import           Data.List (foldl', sort)

type Code = [Int]

type Struct = [[Int]]

type IrreducibleCode = (Int, (Int, Int), (Int, Int))

data Pack = Pack { kValue :: Int
                 , totalValue :: Int
                 , firstSValue :: Int
                 , secondSValue :: Int
                 }

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

toPack :: (Int -> Int) -> Int -> Int -> Pack
toPack calc n k =
  let sk = calc k
      snk = calc (n - k)
  in Pack { kValue = k
          , firstSValue = sk
          , secondSValue = snk
          , totalValue = (2 * k - 1) * sk * snk
          }

-- TODO: use dynamic programming here!
getIrreduciblePacks :: Int -> [Pack]
getIrreduciblePacks
  1 = [Pack { kValue = 1, totalValue = 1, firstSValue = 1, secondSValue = 1 }]
getIrreduciblePacks
  2 = [Pack { kValue = 1, totalValue = 1, firstSValue = 1, secondSValue = 1 }]
getIrreduciblePacks n =
  let elements = [1 .. (n - 1)]
      calc k = sum $ map totalValue $ getIrreduciblePacks k
  in map (toPack calc n) elements

getIrreducibleSearchSpaceSize :: Int -> Int
getIrreducibleSearchSpaceSize k = sum $ map totalValue $ getIrreduciblePacks k

selectPack :: [Pack] -> Int -> (Int, Pack)
selectPack packs m = let t = tail packs
                         h = head packs
                     in if m < totalValue h
                        then (m, h)
                        else selectPack t (m - totalValue h)

findPositions :: (Int, Int, Int) -> (Int, Int, Int)
findPositions (x, sk, snk) =
  let j = x `quot` (sk * snk)
      r = x `rem` (sk * snk)
      a = r `quot` snk
      b = r `rem` snk
  in (j, a, b)

getIrreducibleModelInSpace :: Int -> Position -> IrreducibleCode
getIrreducibleModelInSpace 1 _ = (0, (0, 0), (0, 0)) -- Convention: (1, 0)
getIrreducibleModelInSpace 2 _ = (0, (1, 0), (1, 0))
getIrreducibleModelInSpace n (Position m) =
  let packs = getIrreduciblePacks n
      (x, pack) = selectPack packs m
      (j, a, b) = findPositions (x, firstSValue pack, secondSValue pack)
      k = kValue pack
  in (j, (k, a), (n - k, b))

getIrreducibleCodeToStruct :: IrreducibleCode -> Struct
getIrreducibleCodeToStruct (0, (0, 0), (0, 0)) = [[1, 2]]
getIrreducibleCodeToStruct (j, (k, a), (p, b)) = addIrreducibleStructures
  ( j
  , getIrreducibleCodeToStruct $ getIrreducibleModelInSpace k $ toPosition a
  , getIrreducibleCodeToStruct $ getIrreducibleModelInSpace p $ toPosition b)

getStructFromIndex :: Int -> Position -> Struct
getStructFromIndex n m = getIrreducibleCodeToStruct $ getIrreducibleModelInSpace n m

-- FIXME: What happen when k > 2|x| - 2?
addIrreducibleStructures :: (Int, Struct, Struct) -> Struct
addIrreducibleStructures (_, [], []) = []
addIrreducibleStructures (_, [], y) = y
addIrreducibleStructures (_, x, []) = x
addIrreducibleStructures (k_, x, y) =
  let k = succ k_
      sizeX = 2 * length x
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
