module Day22 (
  day22,
  day22b,
  I(..),
  perform,
  undo,
  solve,
  undoCoeff,
  applyCoeff,
  applyThen,
  applyPower,
) where

import Data.Function ((&))
import Data.List.Split
import qualified Data.Array as Array
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char

import Data.Maybe (catMaybes)
import Text.ParserCombinators.ReadP

import Lib

{-
--- Day 22: Slam Shuffle ---

There isn't much to do while you wait for the droids to repair your ship. At least you're drifting in the right direction. You decide to practice a new card shuffle you've been working on.

Digging through the ship's storage, you find a deck of space cards! Just like any deck of space cards, there are 10007 cards in the deck numbered 0 through 10006. The deck must be new - they're still in factory order, with 0 on the top, then 1, then 2, and so on, all the way through to 10006 on the bottom.

You've been practicing three different techniques that you use while shuffling. Suppose you have a deck of only 10 cards (numbered 0 through 9):

To deal into new stack, create a new stack of cards by dealing the top card of the deck onto the top of the new stack repeatedly until you run out of cards:

Top          Bottom
0 1 2 3 4 5 6 7 8 9   Your deck
                      New stack

  1 2 3 4 5 6 7 8 9   Your deck
                  0   New stack

    2 3 4 5 6 7 8 9   Your deck
                1 0   New stack

      3 4 5 6 7 8 9   Your deck
              2 1 0   New stack

Several steps later...

                  9   Your deck
  8 7 6 5 4 3 2 1 0   New stack

                      Your deck
9 8 7 6 5 4 3 2 1 0   New stack

Finally, pick up the new stack you've just created and use it as the deck for the next technique.

To cut N cards, take the top N cards off the top of the deck and move them as a single unit to the bottom of the deck, retaining their order. For example, to cut 3:

Top          Bottom
0 1 2 3 4 5 6 7 8 9   Your deck

      3 4 5 6 7 8 9   Your deck
0 1 2                 Cut cards

3 4 5 6 7 8 9         Your deck
              0 1 2   Cut cards

3 4 5 6 7 8 9 0 1 2   Your deck

You've also been getting pretty good at a version of this technique where N is negative! In that case, cut (the absolute value of) N cards from the bottom of the deck onto the top. For example, to cut -4:

Top          Bottom
0 1 2 3 4 5 6 7 8 9   Your deck

0 1 2 3 4 5           Your deck
            6 7 8 9   Cut cards

        0 1 2 3 4 5   Your deck
6 7 8 9               Cut cards

6 7 8 9 0 1 2 3 4 5   Your deck

To deal with increment N, start by clearing enough space on your table to lay out all of the cards individually in a long line. Deal the top card into the leftmost position. Then, move N positions to the right and deal the next card there. If you would move into a position past the end of the space on your table, wrap around and keep counting from the leftmost card again. Continue this process until you run out of cards.

For example, to deal with increment 3:


0 1 2 3 4 5 6 7 8 9   Your deck
. . . . . . . . . .   Space on table
^                     Current position

Deal the top card to the current position:

  1 2 3 4 5 6 7 8 9   Your deck
0 . . . . . . . . .   Space on table
^                     Current position

Move the current position right 3:

  1 2 3 4 5 6 7 8 9   Your deck
0 . . . . . . . . .   Space on table
      ^               Current position

Deal the top card:

    2 3 4 5 6 7 8 9   Your deck
0 . . 1 . . . . . .   Space on table
      ^               Current position

Move right 3 and deal:

      3 4 5 6 7 8 9   Your deck
0 . . 1 . . 2 . . .   Space on table
            ^         Current position

Move right 3 and deal:

        4 5 6 7 8 9   Your deck
0 . . 1 . . 2 . . 3   Space on table
                  ^   Current position

Move right 3, wrapping around, and deal:

          5 6 7 8 9   Your deck
0 . 4 1 . . 2 . . 3   Space on table
    ^                 Current position

And so on:

0 7 4 1 8 5 2 9 6 3   Space on table

Positions on the table which already contain cards are still counted; they're not skipped. Of course, this technique is carefully designed so it will never put two cards in the same position or leave a position empty.

Finally, collect the cards on the table so that the leftmost card ends up at the top of your deck, the card to its right ends up just below the top card, and so on, until the rightmost card ends up at the bottom of the deck.

The complete shuffle process (your puzzle input) consists of applying many of these techniques. Here are some examples that combine techniques; they all start with a factory order deck of 10 cards:

deal with increment 7
deal into new stack
deal into new stack
Result: 0 3 6 9 2 5 8 1 4 7

cut 6
deal with increment 7
deal into new stack
Result: 3 0 7 4 1 8 5 2 9 6

deal with increment 7
deal with increment 9
cut -2
Result: 6 3 0 7 4 1 8 5 2 9

deal into new stack
cut -2
deal with increment 7
cut 8
cut -4
deal with increment 7
cut 3
deal with increment 9
deal with increment 3
cut -1
Result: 9 2 5 8 1 4 7 0 3 6

Positions within the deck count from 0 at the top, then 1 for the card immediately below the top card, and so on to the bottom. (That is, cards start in the position matching their number.)

After shuffling your factory order deck of 10007 cards, what is the position of card 2019?
-}

parse ls = ls
         & map parseLine
         & catMaybes

parseLine :: String -> Maybe I
parseLine s =
  case readP_to_S (parser <<<< eof) s of
    [(e, "")] -> Just e
    [] -> Nothing

data I = Cut Integer | Deal Integer | DealNew deriving (Show, Eq)

parser = cutParser +++ dealParser +++ dealNewParser

cutParser = do
  string "cut "
  i <- intParser
  return $ Cut i

dealParser = do
  string "deal with increment "
  i <- intParser
  return $ Deal i

dealNewParser = do
  string "deal into new stack"
  return $ DealNew


day22 ls = do
  let inp = parse ls
      cards = [0..10006]
      final = foldl (\c p -> perform p c) cards inp
      positionsOf = [0..] `zip` final
      answer = positionsOf & filter (\(p, c) -> c == 2019) & head
  putStrLn $ show $ fst answer

perform (Cut n) cards
  | n > 0 = let n' = fromInteger n in drop n' cards ++ take n' cards
  | n < 0 = perform (Cut $ n + (toInteger . length) cards) cards
perform (Deal i) cards =
  let l = length cards
      {- [0, 3, ] -}
      positions = [(c * (fromInteger i)) `mod` l | c <- [0..l - 1]]
      deck = positions `zip` cards
           & Array.array (0, l - 1)
           & Array.elems
  in if gcd i (toInteger l) == 1
     then deck
     else error $ "l = " ++ show l ++ " and increment = " ++ show i
perform (DealNew) cards = reverse cards

{-
After a while, you realize your shuffling skill won't improve much more with merely a single deck of cards.
You ask every 3D printer on the ship to make you some more cards while you check on the ship repairs. While
reviewing the work the droids have finished so far, you think you see Halley's Comet fly past!

When you get back, you discover that the 3D printers have combined their power to create for you a single,
giant, brand new, factory order deck of 119315717514047 space cards.

Finally, a deck of cards worthy of shuffling!

You decide to apply your complete shuffle process (your puzzle input) to the deck 101741582076661 times in a row.

You'll need to be careful, though - one wrong move with this many cards and you might overflow your entire ship!

After shuffling your new, giant, factory order deck that many times, what number is on the card that ends up in
position 2020?

119315717514047
101741582076661
35883736380199
-}

day22b ls =
  let inp = parse ls & reverse
      positionWanted = 2020
      n = 119315717514047
      m = 101741582076661
      n' = 10007
      coeff0 = (1, 0)
      undo1 = foldl (\cf act -> undoCoeff (n - 1) act cf) coeff0 inp
      undom = applyPower (n - 1) undo1 m
      end' = 5755
      start' = 2019
      end = 2020

  in  applyCoeff (n - 1) undom end


{- p -> (a1p + b1) -> (a2(a1p + b1) + b2)-}
applyThen n (a1, b1) (a2, b2) = (a1 * a2 `mod` (n+1), (a2 * b1 + b2) `mod` (n+1))

{- squared: applyThen n (a, b) (a, b) = (a * a `mod` (n+1), (a * b + b) `mod` (n+1)) -}
{- cubed:   applyThen n (a, b) (squared) = (a * a * a, a * a * b + a * b + b) 
            applyThen n (squared) (a, b) = (a * a * a, a * (a * b + b) + b)
                                         = (...      , a * a * b + a * b + b)
                                         ... as we'd hope
            -}



applyPower n (a, b) pow
  | pow == 0 = (1, 0)
  | pow == 1 = (a, b)
  | otherwise =
      let unit = pow `mod` 2
          (a1, b1) = applyPower n (a, b) unit
          (a2, b2) = applyThen n (a, b) (a, b)
          (ah, bh) = applyPower n (a2, b2) (pow `div` 2)
          (ap, bp) = applyThen n (ah, bh) (a1, b1)
          (ap', bp') = applyThen n (a1, b1) (ah, bh)
      in  if (ap, bp) == (ap', bp') then (ap, bp)
          else error $ show ("pow=", pow, "a1,b1", (a1, b1), "ah, bh", (ah, bh), "ap,bp", (ap, bp), "ap'bp'", (ap', bp'))

{-
  dealNew: (ap + b) -> -(ap + b) = (-ap - b)
  cut m: (ap + b) -> ap + b - m = (ap, b-m)
  deal m: (ap + b) -> map + mb = (ma)p + mb
  
  undoing:
  dealNew: (-a, -b)
  cut m: (a, b + m)
  deal m: we want i where im = (ap + b)
          i is (ap + b) * x where xm = 1 (mod n+1) => xm + y(n+1) = 1
          axp + xb
-}

{- undo coefficients: p -> (ap + b) -}
undoCoeff n (DealNew) (a, b) = ((-a) `mod` (n+1), (n - b) `mod` (n+1))
undoCoeff n (Cut m) (a, b) = (a, (b + m) `mod` (n+1))
undoCoeff n (Deal m) (a, b) =
  let (x, y) = solve m (n + 1)
  in ((a * x) `mod` (n+1), (b * x) `mod` (n+1))


applyCoeff n (a, b) p = (a * p + b) `mod` (n + 1)


undo n DealNew p = {- cards are 0..n -> n .. 0 -}
  n - p

undo n (Cut m) p
  = (p + m) `mod` (n + 1)

undo n (Deal m) p =
  {-
     we want i for i * m `mod` (n + 1) = p
     i is p * x if xm = 1 (mod n+1)
                   xm + y(n+1) = 1
  -}
  let (x, y) = solve m (n + 1)
      i = p * x
  in i `mod` (n + 1)


solve a b  {- solve ax + by = 1 -> x, y = (x0, y0) -}
  | b == 1 = (1, 1 - a)
  | otherwise = let (p, q) = a `divMod` b
                    (y0, x0) = solve b q
                    in (x0, y0 - p * x0)
