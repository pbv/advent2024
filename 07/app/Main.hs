--
-- Day 07: Bridge Repair
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
      print input
      putStrLn ("Part1: " ++ show (part1 input))
      putStrLn ("Part2: " ++ show (part2 input))
    _ -> error "invalid arguments"

type Input = [Equation]
type Equation = (Int, [Int])


readInput :: FilePath -> IO Input
readInput path = do
  txt <- readFile path
  return $ map parse (lines txt)
  where
    parse :: String -> Equation
    parse xs = case words xs of
                 (w:ws) -> (read (init w), map read ws)
                 _ -> error "invalid input"

-----------------------------------------------------------------------
part1 :: Input -> Int
part1 = sum . map fst . filter check1


check1 :: Equation -> Bool
check1 (lhs, rhs) = any (==lhs) (values1 $ reverse rhs)

values1 :: [Int] -> [Int]
values1 [x]    = [x]
values1 (x:xs) = [x`op`r | r<-values1 xs, op<-[(+), (*)]]

-----------------------------------------------------------------------
part2 :: Input -> Int
part2  = sum . map fst . filter check2

check2 :: Equation -> Bool
check2 (lhs, rhs) = any (==lhs) (values2 $ reverse rhs)

values2 :: [Int] -> [Int]
values2 [x]    = [x]
values2 (x:xs) = [x`op`r | r<-values2 xs, op<-[(+), (*), concatDigits]]

-- flipped argument order because we reverse the operand list above
concatDigits :: Int -> Int -> Int
concatDigits y x = foldl' (\acc d -> acc*10+d) x (reverse $ digits y)

digits :: Int -> [Int]
digits 0 = []
digits n = (n`mod`10) : digits (n`div`10)


