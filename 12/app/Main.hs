--
-- Day 12: Garden Groups ---
--

module Main where

import System.Environment

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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


-- input types ------------------------------------------------------

type Loc = (Int,Int)

type Input = Map Loc Char  -- the entire garden

type Group = Set Loc -- a garden group:
                     -- set of continuous locations of the same plant



readInput :: FilePath -> IO Input
readInput path = do
  txt <- readFile path
  let input = Map.fromList [ ((i,j),x)
                           | (i,xs) <- zip [0..] (lines txt),
                             (j,x) <-zip [0..] xs
                           ]
  return input

----- part 1

-- common stuff
directions :: Loc -> [Loc]
directions (x,y) = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]

-- find the groups (connected components) of a garden
components :: Input -> [Group]
components input = go input
  where
    go :: Input -> [Group]
    go inp = case Map.lookupMin inp of
              Nothing -> []
              Just (loc,c) -> let r = bfs [loc] c Set.empty
                              in r : go (Map.withoutKeys inp r)
    
    bfs ::  [Loc] -> Char -> Group -> Group
    bfs [] _ acc = acc
    bfs (loc:locs) c acc
      =  let locs' = [ loc' | loc'<-directions loc,
                       loc'`Set.notMember`acc,
                       Map.lookup loc' input == Just c ]
         in bfs (locs'++locs) c (Set.insert loc acc)
    


area :: Group -> Int
area = Set.size

-- | perimeter length of a group
perimeter :: Group -> Int
perimeter r = sum (map exterior $ Set.toList r)
  where
    exterior loc  
      = 4 - length [loc' | loc'<-directions loc, loc'`Set.member`r] 
      

part1 :: Input -> Int
part1 input = sum [area r*perimeter r | r<-components input]
                 
--------------------------------------------------------------------
part2 :: Input -> Int
part2 input = sum [area r*sides r | r<-components input]

-- compute the number of sides (= number of corners) of a group
sides :: Group -> Int
sides r = sum (map corners $ Set.toList r)
  where
    corners :: Loc -> Int
    corners loc = (check upper right loc + check lower right loc +
                   check lower left loc + check upper left loc)
    -- all conditions to check for a corner in vector dir1+dir2 (tricky)
    check dir1 dir2 loc =
      fromEnum (((dir1 loc`Set.member`r == dir2 loc`Set.member`r) &&
                  dir1 (dir2 loc)`Set.notMember`r) ||
                 dir1 (dir2 loc)`Set.member`r &&
                 dir1 loc`Set.notMember`r &&
                 dir2 loc`Set.notMember`r) 

upper :: Loc -> Loc
upper (x,y) = (x,y-1)

lower :: Loc -> Loc
lower (x,y) = (x,y+1)

left :: Loc -> Loc
left (x,y) = (x-1,y)

right :: Loc -> Loc
right (x,y) = (x+1,y)


