--
-- Day 21: Keypad Conundrum
--

module Main where

import           System.Environment
import           Data.List
import           Data.Char

import qualified Data.Map as Map
import           Data.Map (Map, (!))

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      input <- readInput path
      putStrLn ("Part1: " ++ show (part1 input))
      putStrLn ("Part2: " ++ show (part2 input))
    _ -> error "invalid arguments"

type Input = [String]

readInput :: FilePath -> IO Input
readInput path = do
  txt <- readFile path
  return $ lines txt

-- details of the keypads
type Keypad = [String]

numKeypad :: Keypad
numKeypad = ["789", "456", "123", ".0A"]

dirKeypad :: Keypad
dirKeypad = [".^A", "<v>"]

type Vec = (Int,Int)
type Pos = Vec  -- row, column

findPos :: Char -> Keypad -> Pos
findPos c kp
  = head [(i,j) | (i,row)<-zip [0..] kp, (j,x)<-zip [0..] row, x == c]
       
pathsNum :: Char -> Char -> [String]
pathsNum current next
  = let loc = findPos current numKeypad
        loc'= findPos next numKeypad
        avoid = findPos '.' numKeypad     
    in [m ++ "A" | m <- moves loc loc', checkMoves avoid loc m]

pathsDir :: Char -> Char -> [String]
pathsDir current next
  = let loc = findPos current dirKeypad
        loc' = findPos next dirKeypad
        avoid = findPos '.' dirKeypad
    in [m ++ "A" | m <- moves loc loc', checkMoves avoid loc m]

moves :: Pos -> Pos -> [String]
moves (y1,x1) (y2,x2)
  | di/=0 && dj/=0 = [ vert++horiz, horiz++vert ]
  | otherwise = [ vert++horiz ]
  where
    di = y2-y1
    dj = x2-x1
    vert | di>=0 = replicate di 'v'
         | otherwise = replicate (-di) '^'
    horiz | dj>=0 = replicate dj '>'
          | otherwise = replicate (-dj) '<'


-- check that a sequence of moves do not go outside valid keypad positions
checkMoves :: Pos -> Pos -> String -> Bool
checkMoves avoid loc ms = avoid `notElem` listPositions loc ms

listPositions :: Pos -> String -> [Pos]
listPositions = scanl' updatePos 

updatePos :: Pos -> Char -> Pos
updatePos (i,j) c
  = case c of
      'v' -> (i+1,j) 
      '^' -> (i-1,j) 
      '>' -> (i,j+1) 
      '<' -> (i,j-1)
      _ -> error "invalid character"


-- finding the minimum solution
-- memo table for the length of the shortest sub-solution of each level
-- 0: numeric, >0: directional 
type Memo = Map (Int,Char,Char) Int  

-- solve for a number of control keypads
solve :: Int -> String -> Int
solve nkp xs = fst (go 0 Map.empty 'A' xs)
  where
    go :: Int -> Memo -> Char -> String -> (Int, Memo)
    go n memo _ s | n>nkp  = (length s, memo)
    go n memo x s = loop memo 'A' s
      where
        loop :: Memo -> Char -> String -> (Int, Memo)
        loop memo _ [] = (0, memo)
        loop memo x (y:ys)
          | Just s <- Map.lookup (n,x,y) memo =
              let (r, memo') = loop memo y ys
              in (s+r, memo')
          | otherwise =
              let
                (r, memo') = case paths n x y of
                               [p] -> go (n+1) memo 'A' p
                               [p1,p2] ->
                                 let (r1, memo1) = go (n+1) memo 'A' p1
                                     (r2, memo2) = go (n+1) memo1 'A' p2
                                 in if r1<=r2
                                    then (r1, memo2)
                                    else (r2, memo2)
                memo'' = Map.insert (n,x,y) r memo'
                (s, memo''') = loop memo'' y ys
              in
                (r+s, memo''')
              
-- fetch the right paths for a level
paths :: Int -> Char -> Char -> [String]
paths 0 x y = pathsNum x y
paths _ x y = pathsDir x y


 
-- complexity of a sequence with a given number of control keypads
complexity :: Int -> String -> Int
complexity nkp seq = minLen nkp seq * numeric seq

minLen :: Int -> String -> Int
minLen nkp xs = solve nkp xs

numeric :: String -> Int
numeric seq = read (filter isDigit seq)
               
-------------------------------------------------------------
part1 :: Input -> Int
part1 input = sum (map (complexity 2) input)

-------------------------------------------------------------             
part2 :: Input -> Int
part2 input = sum (map (complexity 25) input)

