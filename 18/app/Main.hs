{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}
--
-- Day 18: RAM Run ---
--
module Main where

import           System.Environment
import           Data.List (foldl')
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap


main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      input <- parse <$> readFile path
      putStrLn ("Part1: " ++ show (part1 (take 1024 input)))
      putStrLn ("Part2: " ++ show (part2 input))
    _ -> error "invalid arguments"

-- input types
type Loc = (Int,Int)

type Input = [Loc]   -- corrupted bytes
  
parse :: String -> Input
parse txt
  = map (\xy -> read ("("++xy++")")) (lines txt)


data Memory
  = Memory { corrupted :: Set Loc
           , width :: Int
           , height :: Int
           }
    deriving Show

fillMemory :: [Loc] -> Memory
fillMemory locs
  = Memory { corrupted = Set.fromList locs
           , width = 70
           , height = 70
           }
  
----------------------------------------------------------------------
-- Part 1 uses Dijkstra (similar to day 16)
part1 :: Input -> Maybe Dist
part1 input = let mem = fillMemory input
                  initial = (0,0)
                  final = (mem.width, mem.height)
                  info = dijkstra (makeGraph mem) initial
                  dist = Map.lookup final info.dist
              in dist

----------------------------------------------------------------------
-- Part 2
-- binary partition: iterate the above narrowing the segments of the
-- input until there no longer exists a path
part2 :: Input -> Loc
part2 input = go 1025 (n+1)
  where
    n = length input
    go lo hi
    -- invariant: minimal segment size is >= lo and < hi
      | lo>=hi = input !! (min lo hi)
      | otherwise = let mid = (lo+hi)`div`2
                    in case part1 (take mid input) of
                         Nothing -> go lo (mid-1)
                         Just _ -> go mid hi
                            
    



--------------------------------------------------------------------
-- Dijkstra's algorithm 
-- adapted from Well-Typed Haskell Unfolder presentation:
-- https://github.com/well-typed/unfolder/blob/main/episode020-dijkstras-shortest-paths/Dijkstra.hs
-- modified to use an intmap as a priority queue instead of a list of vertices

type Vertex = Loc
type Dist = Int   -- maxBound for infinity

transitions :: Loc -> [Loc]
transitions (x,y) = [ (x-1,y), (x+1,y), (x,y+1), (x,y-1) ]

data Graph =
  MkGraph
    { vertices   :: [Vertex]
    , neighbours :: Vertex -> [Vertex]
    }



data Info  =
  MkInfo
    { dist :: !(Map Vertex Dist)     -- not in map: INFINITY
    , pqueue :: !(PQ Vertex)
    }
  deriving Show

makeGraph :: Memory -> Graph
makeGraph mem
  = MkGraph { vertices = verts
            , neighbours = neighbours_of
            }
  where
    -- list of all vertices
    verts = [(x,y) | x<-[0..mem.width], y<-[0..mem.height],
              (x,y) `Set.notMember` mem.corrupted ]
    neighbours_of :: Vertex -> [Vertex]
    neighbours_of v = [ v' | v'@(x,y) <- transitions v,
                        0 <= x && x <= mem.width && 
                        0 <= y && y <= mem.height && 
                        v' `Set.notMember` mem.corrupted ]

distanceOf :: Info -> Vertex -> Dist
distanceOf info v 
  =  Map.findWithDefault maxBound v info.dist 

dijkstra :: Graph -> Vertex -> Info 
dijkstra graph source =
  let
    loop :: Info -> Info 
    loop info =
      case minView info.pqueue of
        Nothing ->
          -- we're done!
          info
        Just (u, pq') ->
          let du = distanceOf info u
          in 
          if du < maxBound then
            let
                update :: Info -> Vertex -> Info 
                update i v =
                  let
                    alt = du + 1
                  in
                    case compare alt (distanceOf i v) of
                      LT ->
                        MkInfo
                          (Map.insert v alt i.dist)
                          (addWithPri v alt i.pqueue)
                      EQ ->
                        MkInfo
                          (Map.insert v alt i.dist)
                          i.pqueue 
                      GT ->
                        i 
                info' =
                  foldl' update info{pqueue=pq'} (graph.neighbours u)
              in
                loop info'
            else
              info

  in
    loop 
      (MkInfo
        { dist = Map.singleton source 0
        , pqueue = IntMap.singleton 0 [source]
        }
      )



-- utilities -----------------------------------------------------------
-- use IntMap as a replacement for priority queues
-- (big speedup over naive searching through lists)

type PQ a = IntMap [a]

minView :: PQ a -> Maybe (a, PQ a)
minView pq
  = case IntMap.lookupMin pq of
      Nothing -> Nothing
      Just (_, []) -> Nothing
      Just (_, x:xs) ->
        let pq' = if null xs then
                    IntMap.deleteMin pq
                  else
                    IntMap.updateMin (\_ -> Just xs) pq                    
        in 
          Just (x, pq')
               
addWithPri :: a -> Int -> PQ a -> PQ a
addWithPri x p pq
  = IntMap.insertWith (++) p [x] pq




