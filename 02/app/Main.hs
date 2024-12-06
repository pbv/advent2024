--
-- Day 02: Red-Nosed reports
--

module Main where

import System.Environment
import Data.List

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      input <- readInput path
      putStrLn ("Part1: " ++ show (part1 input))
      putStrLn ("Part2: " ++ show (part2 input))      

    _ -> error "invalid arguments"

type Input = [Report]
type Report = [Int]

readInput :: FilePath -> IO Input
readInput path = do
  txt <- readFile path
  return $ map (map read.words) (lines txt)

-- part 1
part1 :: Input -> Int
part1 reports = length (filter checkSafe reports)

checkSafe :: Report -> Bool
checkSafe levels
  = let deltas = diffs levels
    in (all (>0) deltas || all (<0) deltas) &&
       all (<=3) (map abs deltas)


diffs :: Num a => [a] -> [a]
diffs xs = zipWith (-) xs (tail xs) 


-- part 2
part2 :: Input -> Int
part2 reports = length (filter checkSafe' reports)

checkSafe' :: Report -> Bool
checkSafe' levels =
  let alts = [ xs++ys | (xs,_:ys) <- zip (inits levels) (tails levels)]
  in any checkSafe alts
