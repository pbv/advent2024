--
-- Day 25: Code Chronicle ---
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

type Input = [Block]

readInput :: FilePath -> IO Input
readInput path = do
  txt <- readFile path
  return $ parseBlocks $ lines txt

data Block = Key [Int]
           | Lock [Int]
           deriving Show

parseBlocks :: [String] -> [Block]
parseBlocks [] = []
parseBlocks (first:rest)
  = block (heights (take 5 rest)) : parseBlocks (drop 7 rest)
  where
    block | isEmpty first = Key
          | otherwise = Lock

heights :: [String] -> [Int]
heights = map (length . filter (=='#')) . transpose 

isEmpty :: String -> Bool
isEmpty = all (=='.')

-----------------------------------------------------------------------
part1 :: Input -> Int
part1 blocks 
  = length [ () | Key hs <- blocks, Lock hs'<-blocks, checkFit hs hs']

checkFit :: [Int] -> [Int] -> Bool
checkFit key lock = and [h1+h2 <= 5 | (h1,h2) <- zip key lock]
      
                 
-----------------------------------------------------------------------
part2 :: Input -> Int
part2 input = 0 
