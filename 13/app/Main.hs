--
-- Day 13: Claw Contraption ---
--
{-# LANGUAGE RecordWildCards #-}
module Main where

import System.Environment
import Data.Char
import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      input <- readInput path
      putStrLn ("Part1: " ++ show (part1 input))
      putStrLn ("Part2: " ++ show (part2 input))
    _ -> error "invalid arguments"

-------------------------------------------------------------
type Input = [Puzzle]

type Loc = (Int,Int)
type Vec = (Int,Int)

data Puzzle = Puzzle { buttonA :: Vec
                     , buttonB :: Vec
                     , target :: Loc
                     }
              deriving Show

readInput :: FilePath -> IO Input
readInput path = do
  txt <- readFile path
  case parseInput txt of
    Just lst -> return lst
    Nothing -> error "invalid input"

parseInput :: String -> Maybe Input
parseInput xs = do
  (p, xs) <- parsePuzzle xs
  (do xs <- newline xs    
      ps <- parseInput xs
      return (p:ps)
      <|>
      return [p])


parsePuzzle :: String -> Maybe (Puzzle, String)
parsePuzzle xs = do
  xs<-consume "Button A: " xs
  (vecA,xs) <- parseVec xs
  xs <- newline xs
  xs <- consume "Button B: "xs
  (vecB,xs) <- parseVec xs
  xs <- newline xs  
  xs <- consume "Prize: "xs
  (loc,xs) <- parseLoc xs
  xs <- newline xs    
  return (Puzzle { buttonA = vecA, buttonB = vecB, target= loc }, xs)

parseVec :: String -> Maybe (Vec, String)
parseVec xs = do
  xs <- consume "X+" xs
  (x,xs) <- parseInt xs
  xs<- consume ", Y+" xs
  (y,xs) <- parseInt xs
  return ((x,y),xs)

parseLoc :: String -> Maybe (Loc, String)
parseLoc xs = do
  xs <- consume "X=" xs
  (x,xs)<-parseInt xs
  xs <- consume ", Y=" xs
  (y,xs) <- parseInt xs
  return ((x,y),xs)

parseInt :: String  -> Maybe (Int,String)
parseInt xs@(x:_)
  | isDigit x = let xs' = takeWhile isDigit xs
                    xs''= dropWhile isDigit xs
                in Just (read xs', xs'')
parseInt _ = Nothing

newline :: String -> Maybe String
newline = consume "\n"


consume :: String -> String -> Maybe String
consume str xs
  | str `isPrefixOf` xs = Just (drop (length str) xs)
  | otherwise = Nothing

-------------------------------------------------------------------  

part1 :: Input -> Int
part1 input = sum [3*a+b | Just (a,b) <- map solve_fast input]

-- naive solution, too slow for part 2
solve_naive :: Puzzle -> Maybe Vec
solve_naive Puzzle{..} =
  listToMaybe $
  do a <- [0..100]
     b <- [0..100]
     guard ((a`mulV` buttonA) `addV` (b`mulV` buttonB) == target)
     return (a,b)

addV :: Vec -> Vec -> Vec
addV (x,y) (x',y') = (x+x',y+y')

mulV :: Int -> Vec -> Vec
mulV k (x,y) = (k*x,k*y)


---------------------------------------------------------------------
part2 :: Input -> Int
part2 input = sum [3*a+b | Just (a,b) <- map (solve_fast . adjust) input ]

-- adjust the coordinates of targets
adjust :: Puzzle -> Puzzle
adjust p@Puzzle{..} = p { target = (x + scale, y + scale) }
  where (x,y) = target
        scale = 10000000000000

{-
  solve the system of equations for integer x,y
  x*buttonA + y*buttonB = target
-}
solve_fast :: Puzzle -> Maybe Vec
solve_fast Puzzle{..}
  | x`mod`det == 0 && y`mod`det == 0 = Just (x`div`det, y`div`det)
  | otherwise = Nothing
  where
    x = by*tx-bx*ty
    y = -ay*tx+ax*ty 
    det = ax*by - bx*ay
    (ax,ay) = buttonA
    (bx,by) = buttonB
    (tx,ty) = target
        
  
