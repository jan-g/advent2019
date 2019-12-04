module Day4 where

import Data.Function ((&))
import Data.List.Split
import Data.Array
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char

{-
--- Day 4: Secure Container ---

You arrive at the Venus fuel depot only to discover it's protected by a password. The Elves had written the
password on a sticky note, but someone threw it out.

However, they do remember a few key facts about the password:

    It is a six-digit number.
    The value is within the range given in your puzzle input.
    Two adjacent digits are the same (like 22 in 122345).
    Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123
    or 135679).

Other than the range rule, the following are true:

    111111 meets these criteria (double 11, never decreases).
    223450 does not meet these criteria (decreasing pair of digits 50).
    123789 does not meet these criteria (no double).

How many different passwords within the range given in your puzzle input meet these criteria?
-}

parse ls = ls
         & head
         & splitOn "-"
         & map read

day4 ls =
  let [from, to] = parse ls
  in  length [x | x <- [from .. to], criterion x]

criterion :: (Num a, Show a) => a -> Bool
criterion n =
  let ds = n & show & map digitToInt :: [Int]
  in  scan (-1) False ds
  where
    scan :: Int -> Bool -> [Int] -> Bool
    scan _ double [] = double
    scan prev double (x:xs)
         | prev > x  = False
         | prev == x = scan x True xs
         | otherwise = scan x double xs 

{-
An Elf just remembered one more important detail: the two adjacent matching digits are not part of a larger group of matching digits.

Given this additional criterion, but still ignoring the range rule, the following are now true:

    112233 meets these criteria because the digits never decrease and all repeated digits are exactly two digits long.
    123444 no longer meets the criteria (the repeated 44 is part of a larger group of 444).
    111122 meets the criteria (even though 1 is repeated more than twice, it still contains a double 22).

How many different passwords within the range given in your puzzle input meet all of the criteria?
-}

day4b ls =
  let [from, to] = parse ls
  in  length [x | x <- [from .. to], criterionb x]

criterionb :: (Num a, Show a) => a -> Bool
criterionb n =
  let ds = n & show & map digitToInt :: [Int]
  in  scan (-1) 1 False ds
  where
    scan :: Int -> Int -> Bool -> [Int] -> Bool
    scan _ nPrev double [] = double || (nPrev == 2)
    scan prev nPrev double (x:xs)
         | prev > x  = False
         | prev < x  = scan x 1 (double || nPrev == 2) xs
         | prev == x = scan x (nPrev + 1) double xs
         | otherwise = scan x 1 double xs 
