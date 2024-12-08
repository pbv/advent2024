--
-- Day 8: Resonant Collinearity 
--
{-# LANGUAGE RecordWildCards #-}
module Main where

import System.Environment
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      input <- readInput path
      putStrLn ("Part1: " ++ show (part1 input))
      putStrLn ("Part2: " ++ show (part2 input))
    _ -> error "invalid arguments"


--- input types

type Loc = (Int,Int)

type Freq = Char

data Grid = Grid { width :: Int
                 , height :: Int
                 , antennas :: Map Freq [Loc]
                 } deriving Show

type Input = Grid

readInput :: FilePath -> IO Input
readInput path = do
  xss <- lines <$> readFile path
  let locs = Map.fromListWith (++) [(x, [(i,j)])
                                   | (i,xs) <- zip [0..] xss,
                                     (j,x) <- zip [0..] xs, x/='.']
  let h = length xss
  let w = length (xss!!0)
  return $ Grid { width = w
                , height = h
                , antennas = locs
                }
    
-----------------------------------------------------------------------------
-- common bits
----------------------------------------------------------------------------
inside :: Grid -> Loc -> Bool
inside Grid{..} (x,y) = x>=0 && y>=0 && x<height && y<width

antinodes :: [Int] -> Grid  -> Set Loc
antinodes factors grid@Grid{..} 
  = Set.unions $ Map.elems $
    Map.map (\locs -> Set.fromList (ressonances factors grid locs)) antennas

ressonances ::  [Int] -> Grid -> [Loc] -> [Loc]
ressonances factors grid  = go 
 where
   go [] = []
   go (loc:locs) = [loc'' | loc'<-locs, loc''<-echoes loc loc'] ++  go locs
   
   echoes (x,y) (x',y')
     = takeWhile (inside grid)  [(x'+k*dx, y'+k*dy) | k<-factors] ++
       takeWhile (inside grid)  [(x-k*dx, y-k*dy) | k<-factors]
     where dx = x'-x
           dy = y'-y


------------------------------------------------------------------------
part1 :: Input -> Int
part1 = Set.size . antinodes [1] 

-------------------------------------------------------------------------
part2 :: Input -> Int
part2 = Set.size . antinodes [0..]

