--
-- Day 03: Mull it over
-- 
{-# LANGUAGE BangPatterns #-}
module Main where

import System.Environment
import Data.Char
import Data.List
import Control.Applicative((<|>))

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      input <- parseInput <$> readFile path
      putStrLn ("Part1: " ++ show (part1 input))
      putStrLn ("Part2: " ++ show (part2 input))
    _ ->
      error "invalid arguments"

-- part1 and 2
type Input = [Op]

data Op = Mul Int Int  -- mul(X,Y)
        | Do           -- do()
        | Dont         -- don't()
        deriving Show

parseInput :: String -> Input
parseInput [] = []
parseInput xs
  = case parseOp xs of
      Just (op, xs') -> op : parseInput xs'
      Nothing -> parseInput (tail xs)


parseOp :: String -> Maybe (Op, String)
parseOp [] = Nothing
parseOp xs
  = do xs0 <- consume "mul(" xs
       (a, xs1) <- parseInt xs0
       xs2 <- consume "," xs1
       (b, xs3) <- parseInt xs2
       xs4 <- consume ")" xs3
       return (Mul a b, xs4)
  <|> do xs0 <- consume "do()" xs
         return (Do, xs0) 
  <|> do xs0 <- consume "don't()" xs
         return (Dont,xs0)

        


parseInt :: String -> Maybe (Int, String)
parseInt [] = Nothing
parseInt xs@(h:_)
  | isDigit h = let xs' = takeWhile isDigit xs
                    xs''= dropWhile isDigit xs
                    in Just (read xs', xs'')
  | otherwise = Nothing

consume :: String -> String -> Maybe String
consume str xs | str`isPrefixOf`xs = Just (drop (length str) xs)
               | otherwise = Nothing

--- part 1 
part1 :: Input -> Int
part1 input = sum (map mult input)
  
mult :: Op -> Int
mult (Mul x y) = x*y
mult _ = 0


-- part 2
type State = (Bool,Int)

part2 :: Input -> Int
part2 input = let  initial = (True, 0)
              in snd (foldl' compute initial input)

compute :: State -> Op -> State
compute (flag, !acc) (Mul x y)
  | flag      = (flag, acc+x*y)
  | otherwise = (flag, acc)
compute (_, acc) Do
  = (True, acc)
compute (_, acc) Dont
  = (False, acc)

