module Random where

import Data.Bits

randomNumbers :: Int -> [Int]
randomNumbers seed = map mask $ randomNumberSequence [seed..]

randomNumberSequence :: [Int] -> [Int]
randomNumberSequence l = case l of
  [] -> []
  l : _ : ls -> l : randomNumberSequence (nextRandomNumber l : ls)
  l : ls -> l : ls

modulus :: Int
modulus = 2 ^ 32

multiplier :: Int
multiplier = 1103515245

increment :: Int
increment = 12345

nextRandomNumber :: Int -> Int
nextRandomNumber previousNumber =
  (multiplier * previousNumber + increment) `mod` modulus

mask :: Int -> Int
mask x = shift (sum (map bit [16..30]) .&. x) (-16)
