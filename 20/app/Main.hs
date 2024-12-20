{-# LANGUAGE OverloadedRecordDot #-}
--
-- Day 20: Race Condition ---
--
module Main where

import System.Environment
import Data.List
import qualified Data.Map as Map
import           Data.Map (Map, (!))
import qualified Data.IntMap as IntMap
import           Data.IntMap (IntMap)
import qualified Data.Set as Set
import           Data.Set (Set)

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

data Track
  = Track { walls :: Set Loc
          , start :: Loc
          , end :: Loc
          , width :: Int
          , height :: Int
          } deriving Show

type Input = Track

readInput :: FilePath -> IO Input
readInput path
  = parse . lines  <$> readFile path

parse :: [String] -> Track
parse xss =
  Track { walls = Set.fromList (find '#')
        , start = head (find 'S')
        , end = head (find 'E')
        , width = length (head xss)
        , height = length xss
        }
  where
    find ch = [(x,y) | (y,xs)<- zip [0..] xss,
                       (x,ch') <- zip [0..] xs, ch'==ch ]
                                 


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
    , prev :: !(Map Vertex Vertex)  
    , pqueue :: !(PQ Vertex)
    }
  deriving Show

makeGraph :: Track -> Graph
makeGraph track
  = MkGraph { vertices = verts
            , neighbours = neighbours_of
            }
  where
    -- list of all vertices
    verts = [(x,y) | x<-[0..track.width-1], y<-[0..track.height-1],
              (x,y) `Set.notMember` track.walls ]
    neighbours_of :: Vertex -> [Vertex]
    neighbours_of v = [ v' | v'@(x,y) <- transitions v,
                        0 <= x && x < track.width && 
                        0 <= y && y < track.height && 
                        v' `Set.notMember` track.walls ]

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
                     if alt < distanceOf i v then
                        MkInfo
                          (Map.insert v alt i.dist)
                          (Map.insert v u i.prev)
                          (addWithPri v alt i.pqueue)
                     else
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
        , prev = Map.empty
        , pqueue = IntMap.singleton 0 [source]
        }
      )


shortestPath :: Info -> Vertex -> [Vertex]
shortestPath info = go
  where
    go v = v : case Map.lookup v info.prev of
              Just v' -> go v'
              Nothing -> []

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

-----------------------------------------------------------------
part1 :: Input -> Int
part1 track
  = let info = dijkstra (makeGraph track) track.start
        path = Set.fromList (shortestPath info track.end)
        candidates = Map.fromListWith (+)
                     [(d,1) | v<-Set.toList track.walls,
                       v' <- transitions v,
                       v' `Set.member` path, 
                       v'' <- transitions v,
                       v' < v'',
                       v'' `Map.member` info.dist,
                       let d =  abs (info.dist!v' - info.dist!v'') - 2,
                       d>=100 ]
    in sum candidates





--------------------------------------------------------                 
part2 :: Input -> Int
part2 input = 0 
