module Exercises where

import Data.Array.Partial as P
import Control.MonadZero (guard)
import Data.Array (concat, concatMap, filter, foldl, head, length, tail, (..))
import Data.Foldable (product)
import Data.Maybe (maybe)
import Prelude (bind, map, pure, ($), (&&), (*), (+), (-), (/), (/=), (<), (<$>), (<<<), (<=), (==), (>=), (||))

isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven n =
    if n < 0
       then isEven (n + 2)
       else isEven (n - 2)

countEven :: Array Int -> Int
countEven [] = 0
countEven arr = countFromHead + countFromTail
    where
        countFromHead :: Int
        countFromHead = maybe 0 (boolToInt <<< isEven) (head arr)
        countFromTail :: Int
        countFromTail = maybe 0 countEven $ tail arr

boolToInt :: Boolean -> Int
boolToInt false = 0
boolToInt true = 1

toSquares :: Array Int -> Array Int
toSquares = (<$>) (\n -> n * n)

trimNegatives :: Array Int -> Array Int
trimNegatives = (<$?>) (\n -> n < 0)

factors :: Int -> Array (Array Int)
factors n = do
    i <- 1 .. n
    j <- i .. n
    guard $ i * j == n
    pure [i, j]

isPrime :: Int -> Boolean
isPrime = (\x -> x == 1) <<< length <<< factors

cartesianProduct :: Array Int -> Array Int -> Array (Array Int)
cartesianProduct left right = do
    i <- left
    j <- right
    pure [i, j]

triples :: Int -> Array (Array Int)
triples n = do
    i <- 1 .. (n / 2)
    j <- i .. (n / 2)
    let sq = i * i + j * j
    guard $ sq < n
    pure [i, j, sq]

factorizations :: Int -> Array (Array Int)
factorizations n = concat [[[1, n]], (factorizations' n 2)]

factorizations' :: Int -> Int -> Array (Array Int)
factorizations' 0 _ = []
factorizations' 1 _ = []
factorizations' 2 _ = []
factorizations' 3 _ = []
factorizations' n min = do
    i <- 2 .. (n / 2)
    let j = n / i
    guard $ i >= min && i >= j
    guard $ i * j == n
    let concattedTail = map (\xs -> concat [[i], xs])
            $ factorizations' j i
    concat [concattedTail, [[i, j]]]

allTrue :: Array Boolean -> Boolean
allTrue = foldl (&&) true

infix 8 filter as <$?>
