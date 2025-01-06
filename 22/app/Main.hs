--
-- Day 22: Monkey Market ---
--
module Main where

import System.Environment
import Data.List
import Data.Bits

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      input <- readInput path
      putStrLn ("Part1: " ++ show (part1 input))
      putStrLn ("Part2: " ++ show (part2 input))
    _ -> error "invalid arguments"

type Input = [Int]

readInput :: FilePath -> IO Input
readInput path = do
  txt <- readFile path
  return $ map read (lines txt)

--------------------------------------------------

next :: Int -> Int
next = step3 . step2 . step1
  where step1 x = prune (mix x (x*64))
        step2 x = prune (mix x (x`div`32))
        step3 x = prune (mix x (x*2048))
        mix = xor 
        prune x = x `mod` 16777216

--------------------------------------------------
part1 :: Input -> Int
part1 input = sum (map (\x -> iterate next x !! 2000) input)


part2 :: Input -> Int
part2 input = 0 
