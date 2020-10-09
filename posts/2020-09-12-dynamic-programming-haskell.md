---
title: Dynamic Programming in Haskell
---

Last week, [Eric](https://emayhew.com/) came to me with a fun programming
challenge: the knight's tour around the keypad. This by now a well-known
problem. I had actually been given it once upon a time in undergrad when
interviewing for an internship. Alas back then, I couldn't solve it, and I
didn't get the internship. In this blog post, I want to set the record straight,
and show off how to solve it using Haskell.

## The knight's tour

The problem is this: if we have a phone keypad such as

```
1 2 3
4 5 6
7 8 9
  0
```

and a knight chess piece positioned on a given number, then how many phone
numbers can the knight dial in `N` moves?

The knight can only move in a chess-legal way. For example, if the knight is on
`1`, then it can move only to `8` and `6`.

### A first approach

To get a feel for the solution, let's imagine that we have `4` units of fuel to
move with and that the knight is on the `1` key. If we know how many numbers
could be dialed with `3` units of fuel starting on `1`'s neighbours, `6` and
`8`, then we could just add those together to obtain the count of numbers that
can be dialed in `4` units starting on `1`. And what happens when we have no
fuel left? We can dial exactly one number -- the empty number.

The above suggests a straightforward recursive solution. The only difficulty is
in determining the neighbours for a given key. Luckily we can write a very
simple function to give us just that information.

```
module KnightTour where

neighbours :: Int -> [Int]
neighbours n = case n of
  0 -> [4, 6]
  1 -> [6, 8]
  2 -> [7, 9]
  3 -> [4, 8]
  4 -> [3, 9, 0]
  5 -> []
  6 -> [1, 7, 0]
  7 -> [2, 6]
  8 -> [1, 3]
  9 -> [4, 2]
```

Now we can write the recursive solution.

```
countPaths1 :: Int -> Int -> Int
countPaths1 0 _ = 1
countPaths1 fuel key = sum $ map (countPaths1 $ fuel - 1) (neighbours key)
```

The problem with this solution is that it runs in exponential time: except in
the base case, we make at least 2 recursive calls, so if we draw a tree of all
the recursive calls, it will have at least `2^n` nodes, where `n` is the amount
of fuel.

### Dynamic programming

We can significantly improve this algorithm by observing that there is a lot of
redundant calculation made in it. For example, let's say we're on the `8` key
with `n` units of fuel. We need to make the following calculations.

```
countPaths1 n 8 = countPaths1 (n-1) 1 + countPaths1 (n-1) 3
| countPaths1 (n-1) 1 = countPaths (n-2) 6 + countPaths1 (n-2) 8
| | countPaths1 (n-2) 6 = countPaths1 (n-3) 0 + countPaths1 (n-3) 1 + countPaths (n-3) 7
| | countPaths1 (n-2) 8 = ...
| countPaths1 (n-1) 3 = countPaths (n-2) 4 + countPaths1 (n-2) 8
| | countPaths1 (n-2) 4 = countPaths1 (n-3) 0 + countPaths1 (n-3) 3 + countPaths (n-3) 9
| | countPaths1 (n-2) 8 = ...
```

We can see that the calculations for `countPaths1 (n-2) 8` and `countPaths1
(n-3) 0` both occur twice.

To avoid recalculating anything, we can turn our implementation around. Instead
of breaking apart the problem into subproblems to be solved recursively, we can
_build up_ the solution starting from the base case. This technique is called
_dynamic programming_.

To use dynamic programming,
we tend to need to generalize the problem: rather than just consider the count
of numbers that can be dialed starting on *one* key, we compute the count of
numbers that can be dialed starting on *each* key. Let's call this collection of
counts a _stage_ of the algorithm.

To see that this speeds up the algorithm and eliminates the redundancy from the
previous solution, let's suppose that we know the `N`th stage. That is, we have
some list `L` such that indexing it as `L[K]` gives the answer to the question
"how many numbers can be dialed starting on key `K` with `N` units of fuel?"
(Note `N` is fixed here.)
Then, we can easily compute the `N+1`th stage `L'` such that for each `K'`,
`L'[K']` is precalculated as the sum of `L[K]` for each neighbour `K` of `K'`.
The initial stage `L0` sends every key to `1`.

Now let's encode this in Haskell.

```
type Stage = [Int]

stage0 :: Stage
stage0 = map (const 1) [0..9]

next :: Stage -> Stage
next l = map (sum . map (l !!) . neighbours) [0..9]
```

Now if we want to know how many numbers can be dialed starting on `K` with `N`
units of fuel, then we let `next^N` be `next` composed with itself `N` times and
calculate `next^N stage0 !! K`.

We can generalize a touch further using the following function from the standard
library.

```
iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)
```

This function constructs an infinite list such that if `L = iterate f x`, then
`L[N] = f^N x` where `f^N` is `f` composed with itself `N` times.
In other words, the list looks like `[x, f x, (f . f) x, (f . f . f) x, ...]`.

This allows us to define the following.

```
allStages :: [Stage]
allStages = iterate next stage0
```

And we can answer the original problem -- how many numbers can be dialed
starting on `K` with `N` units of fuel? -- by indexing twice.

```
countPaths2 :: Int -> Int -> Int
countPaths2 fuel key = allStages !! fuel !! key
```

Now this algorithm runs in linear time in the amount of fuel `N`.

To recap, we converted an ordinary recursive algorithm to use dynamic
programming by turning it around. Instead of breaking down the problem into
subproblems, We generalized the problem to consider
_stages_, and built up to the stage we needed.
To do this, we identified an initial stage and defined a function that given one
stage gives us the next stage.

## Eight Queens

Now that we've adequately warmed up, we can tackle another famous dynamic
programming problem: the eight queens problem.
In this challenge, we want to place 8 queens on a chessboard so that no two
queens could attack each other.

This time around, we won't bother with writing an ordinary recursive function
first and converting it. In fact, it's not always as illuminating as in the
previous problem to do so. (The reason it's less helpful this time is that the
ordinary recursive solution is a backtracking search, whereas the dynamic
programming solution is more direct.)

Instead, we'll figure out what a stage is in this algorithm, what the initial
stage looks like, and how to calculate the next stage from the current stage.

Since we will need to decide whether tiles are in danger of attack by a queen,
we will need to reason about and manipulate boards, so let's encode them.

```
module EightQueens where

import Data.Tuple ( swap )
import Data.List ( sort, uniq )

type Tile = Queen | Safe | Danger
  deriving (Eq, Ord, Show)

type Board = [Tile]

boardWidth, boardLength :: Int
boardWidth = 8
boardLength = boardWidth^2

indexToVec :: Int -> (Int, Int)
indexToVec = swap . (`divMod` boardWidth)
```

A board is just a list of tiles, and a tile either contains a queen, is safe for
a queen, or is dangerous for a queen.
And what is a stage in this algorithm? Let's imagine we want to put down the
eighth queen. If only we knew _all_ the boards with _seven_ queens on them, then
we could find all the safe positions in those boards and put a queen on each.
So a stage `N` in this algorithm is the list of all boards with `N` queens.

The initial stage (stage 0) is simply a list containing only the empty board.

```
type Stage = [Board]

stage0 :: Stage
stage0 = [emptyBoard]

emptyBoard :: Board
emptyBoard = map (const Empty) [1..boardLength]
```

Next, we need to think about how to get the next stage from the current
stage: for each board `B` in the current stage, generate
a new board `B'` for each empty position `P` in `B`, such that `B'[P] = Queen`
and `B'[P'] = Danger` if position `P'` is in danger of position `P`.

This suggests a small piece of functionality that we need -- we need to be able
to place a queen on a board, putting `Danger` on all the tiles that queen
can reach. And to implement this small piece of functionality, we need the
ability to traverse a board and construct a new one. This traversal needs to be
aware of the coordinates of the tiles being processed, because whether a tile is
in danger depends on the relationship of its coordinates to those of the queen
being placed.

```
imapBoard :: ((Int, Int) -> Tile -> a) -> Board -> [a]
imapBoard f = zipWith f indices where
  indices = map indexToVec [0..]

placeQueen :: (Int, Int) -> Board -> Board
placeQueen pos = imapBoard f where
  f pos' tile
    | pos' == pos = Queen
    | pos' `inDangerOf` pos = Danger
    | otherwise = tile
```

All that's missing from the above snippet is `inDangerOf`. Two positions are in
danger of each other if they have the same x coordinate (vertical attack), the
same y coordinate (horizontal attack),
or their absolute x coordinate difference equals their absolute y coordinate
difference (diagonal attack).

```
inDangerOf :: (Int, Int) -> (Int, Int) -> Bool
(x, y) `inDangerOf` (qx, qy) = x == qx || y == qy || (qx - x)^2 == (qy - y)^2
```

Now we can loop back to the game plan: place a queen on every `Empty` tile in a
board to generate new boards.

```
placeQueens :: Board -> [Board]
placeQueens b = map (\pos -> placeQueen pos b) $ empties b

empties :: Board -> [(Int, Int)]
empties = map fst . filter (\(_, tile) -> tile == Empty) . imapBoard (,)
```

Now we apply `placeQueens` to every board in the stage, concatenating the
results to form the next stage.

```
next :: Stage -> Stage
next = concatMap placeQueens
```

There's one small problem with this implementation: the same board can be
generated in different ways, so we end up with duplicate boards in our next
stage.
To see this, imagine that in stage 2 we have the boards `B1` and `B2` and
the positions `A`, `B` and `C` such that:

* `B1[A] = Queen` and `B2[A] = Empty`
* `B1[B] = Empty` and `B2[B] = Queen`
* `B1[C] = Queen` and `B2[C] = Queen`

`B1` will give rise to a new board `B1'` in the next stage with `B1'[B] = Queen`
and `B2` will give rise to a new board `B2'` in the next stage with `B2'[A] =
Queen`. The upshot is that after calling `next stage2` to obtain stage 3, we
have two identical boards in that new stage!

The right thing to do is to avoid generating the duplicate boards in the first
place, which is left as an exercise to the reader. Instead, we'll just remove
the duplicate boards after generating the next stage.

```
next :: Stage -> Stage
next = uniq . sort . concatMap placeQueens
```

As before, we can get the answer by iterating `next` and indexing.

```
answer :: Board
answer = head $ iterate next stage0 !! 8
```

And maybe we want to see what this board actually looks like.

```
main :: IO ()
main = putStrLn (drawBoard answer)

drawBoard :: Board -> String
drawBoard = concatMap drawRow . groupBySize boardWidth where
  drawRow = (++ "\n") . concatMap drawTile where
    drawTile t = case t of
      Queen -> "Q "
      Danger -> "_ "
      Empty -> "_ "

groupBySize :: Int -> [a] -> [[a]]
groupBySize _ [] = []
groupBySize n l = xs : groupBySize n xss where
  (xs, xss) = splitAt n l
```

To recap, we thought about what a stage is and figured out what the initial
stage is. Then, we implemented a function to get the next stage given the
current stage. Both examples discussed in this article were the same in that
sense: we only needed the immediately previous stage. In general, we might need
access to several previous stages. Or, as we saw in the Eight Queens problem, we
might need to eliminate redundancy within the calculation of the next stage.
Regardless, dynamic programming is a powerful technique that lends itself
surprisingly well to implementation in functional languages, despite commonly
being taught using hashtables and memoization, both of which are more suited to
imperative implementations.
