--
-- Day 10: Hoof It ---
--
module Main where

import           System.Environment
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


-- input types
type Loc = (Int,Int)
type Input = Map Loc Int -- map values are heights 0 .. 9

readInput :: FilePath -> IO Input
readInput path = do
  xss <- readFile path
  return $ Map.fromList  [((i,j),read [x])
                         | (i,xs)<-zip [0..] (lines xss)
                         , (j,x)<-zip [0..] xs
                         , '0'<=x && x<='9']

---------------------------------------------------------------------
part1 :: Input -> Int
part1 input =
  sum $ map (score input) (trailheads input)

trailheads :: Input -> [Loc]
trailheads input = Map.keys (Map.filter (==0) input)

score :: Input -> Loc -> Int
score input start = Set.size (dfs1 input start)

-- DFS to find all maximum height location from a given trailhead
dfs1 :: Input -> Loc -> Set Loc
dfs1 input start = go start 0
  where
    go loc h
      | h == 9 = Set.singleton loc
      | otherwise
      = let h' = h+1
        in 
          Set.unions [go loc' h'
                     | loc' <- directions loc
                     , Just h' == Map.lookup loc' input 
                     ]
  
directions :: Loc -> [Loc]
directions (x,y) = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
              
---------------------------------------------------------------------
part2 :: Input -> Int
part2 input =
  sum $ map (rating input) (trailheads input)


-- compute the rating of a start location
-- DFS to find the number hiking trails from a starting location
rating :: Input -> Loc -> Int
rating input start = go start 0
  where
    go loc h
      | h == 9 = 1
      | otherwise
      = let h' = h+1
        in 
          sum [go loc' h'
              | loc' <- directions loc
              , Just h' == Map.lookup loc' input 
              ]
