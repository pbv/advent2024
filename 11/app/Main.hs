--
-- Day 11: Plutonian Pebbles ---
--
{-# LANGUAGE BangPatterns #-}

module Main where

import System.Environment

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Test.QuickCheck

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      input <- readInput path
      print input
      putStrLn ("Part1: " ++ show (part1 input))
      putStrLn ("Part2: " ++ show (part2 input))
    _ -> error "invalid arguments"

type Input = [Int]

readInput :: FilePath -> IO Input
readInput path = do
  txt <- readFile path
  return (map read $ words txt)


-- | apply a function n times
applyMany :: Int -> (a -> a) -> a -> a
applyMany 0 _  x = x
applyMany n f !x = applyMany (n-1) f (f x)


----------------------------------------------------
-- | naive solution: computes the list of final stones
-- (too slow for part 2)
solve_naive :: Int -> Input -> Input
solve_naive n input = applyMany n (concatMap blink) input 

-- change a single stone
blink :: Int -> [Int]
blink n
  | n == 0 = [1]
  | even k = let a = n `div` (10^(k`div`2))
                 b = n `mod` (10^(k`div`2))
             in [a,b]
  | otherwise = [2024*n]
  where k = digits n

-- number of digits (n>0)
digits :: Int -> Int
digits n = loop n 0
  where
    loop 0 acc = acc
    loop k !acc = loop (k`div`10) (1+acc)


-- | dynamic programming approach
-- computes just the number of stones
solve_memo :: Int -> Input -> Int
solve_memo n input = sum (IntMap.elems memo)
  where memo = applyMany n blinks (IntMap.fromListWith (+) [(x,1)|x<-input])

blinks :: IntMap Int -> IntMap Int
blinks memo
  = IntMap.fromListWith (+)
    [ (next,n) | (stone,n)<-IntMap.assocs memo, next<-blink stone]


-------------------------------------------------------------------

part1 :: Input -> Int
part1 input = solve_memo 25 input  -- == length (solve_naive 25 input)

part2 :: Input -> Int
part2 input = solve_memo 75 input

-------------------------------------------------------------------
-- Quickcheck properties
-------------------------------------------------------------------
genInput :: Gen Input
genInput = listOf arbitrarySizedNatural

-- | distributivity i.e., the problem has an optimal substructure
prop_optimal :: Property
prop_optimal
  = forAll ((,) <$> genInput <*> genInput) $ \(xs,ys) ->
    forAll (choose (1,10)) $ \n ->
    solve_naive n (xs ++ ys) === solve_naive n xs ++ solve_naive n ys

-- | the memorized version computes the correct number of stones
prop_solve :: Property
prop_solve  =
  forAll (choose (1,10)) $ \n ->
  forAll genInput $
  \input -> length (solve_naive n input) === solve_memo n input


