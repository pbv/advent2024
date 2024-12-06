--
-- Day 01: Historian Hysteria
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

type Input = [(Int, Int)]

readInput :: FilePath -> IO Input
readInput path = do
  txt <- readFile path
  return $ map parse (lines txt)
  where
    parse :: String -> (Int, Int)
    parse xs = case words xs of
                 [x,y] -> (read x, read y)
                 _ -> error "invalid input"


part1 :: Input -> Int
part1 input = let xs = sort (map fst input)
                  ys = sort (map snd input)
              in sum (zipWith distance xs ys)

distance :: Int -> Int -> Int
distance x y = abs (x-y)
                 

part2 :: Input -> Int
part2 input = let xs = map fst input
                  ys = map snd input
              in sum [x*c | x <- xs, let c = count x ys]

count :: Eq a => a -> [a] -> Int
count x ys = length (filter (==x) ys)                  
