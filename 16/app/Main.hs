{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}
--
--- Day 16: Reindeer Maze ---
--
module Main where

import           System.Environment
import           Data.List
import           Data.Ord
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      maze <- parse <$> readFile path
      let info =  dijkstra (makeGraph maze) (maze.start,E)
      let part1 = minimum [Map.findWithDefault maxBound (maze.end,dir) info.dist | dir <- [N,S,E,W]] 
      let part2 = Set.size $ Set.unions $ map Set.fromList $ pathsFrom info (maze.end,N)
      putStrLn ("Part1: " ++ show part1)
      putStrLn ("Part2: " ++ show part2)
    _ -> error "invalid arguments"

-- input types

type Loc = (Int,Int)

data Dir = N | W | S | E deriving (Eq, Ord, Enum, Show)
-- ordered in CCW rotation

data Maze = Maze { walls :: Set Loc
                 , width :: Int
                 , height :: Int
                 , start :: Loc
                 , end :: Loc
                 }
            deriving Show

type Input = Maze

parse :: String -> Maze
parse txt 
  = let objs = [ ((i,j),x) | (i,xs) <- zip [0..] (lines txt)
                           , (j,x) <- zip [0..] xs
                           , x `elem`"#SE" ]
        s = head [ loc | (loc, 'S') <- objs ]
        e = head [ loc | (loc, 'E') <- objs ]
        w = Set.fromList [ loc | (loc, '#') <- objs ]
        hh = length (lines txt)
        ww = length (head (lines txt))
    in Maze { walls = w, start = s, end = e,
              width = ww, height = hh
            }

----------------------------------------------------------------------

type State = (Loc, Dir)     -- the reindeer's state

forward :: Loc -> Dir -> Loc
forward (!x,!y) dir
  = case dir of
      N -> (x-1,y)
      S -> (x+1,y)
      E -> (x,y+1)
      W -> (x,y-1) 

turnCCW :: Dir -> Dir
turnCCW !dir = toEnum ((fromEnum dir + 1)`mod`4)

turnCW :: Dir -> Dir
turnCW !dir =  toEnum ((fromEnum dir - 1)`mod`4)

transitions :: State -> [(State,Dist)]
transitions (loc,dir) = [ ((forward loc dir, dir), 1)
                        , ((loc, turnCCW dir),     1000)
                        , ((loc, turnCW dir),      1000) ]


makeGraph :: Maze -> Graph
makeGraph maze
  = MkGraph { vertices = states
            , neighbours = neighbours_of
            , cost = cost_of
            }
  where
    locs = Set.fromList [ (x,y) |
                            x <- [0.. maze.height -1]
                          , y <- [0.. maze.width - 1]
                          , (x,y) `Set.notMember` maze.walls ]
    states = [ (loc,dir) | loc <- Set.toList locs, dir<-[N,S,E,W] ]

    neighbours_of :: State -> [State]
    neighbours_of s = [ s' | (s',_) <- transitions s, fst s'`Set.member`locs ]
    cost_of :: State -> State -> Dist
    cost_of s s' = head [ c | (s'',c) <- transitions s, s' == s'' ]
              


-- Dijkstra's algorithm 
data Graph =
  MkGraph
    { vertices   :: [State]
    , neighbours :: State -> [State]
    , cost       :: State -> State -> Dist
    }

type Dist = Int   -- maxBound for infinity

data Info  =
  MkInfo
    { dist :: !(Map State Dist)     -- not in map: INFINITY
    , prev :: !(Map State [State])  -- not in map: UNDEFINED
    }
  deriving Show


minViewOn :: (State -> Dist) -> [State] -> Maybe (State, [State])
minViewOn _ [] = Nothing
minViewOn f xs =
  let
    m = minimumBy (comparing f) xs
  in
    Just (m, delete m xs)

distanceOf :: Info -> State -> Dist
distanceOf info v 
  =  Map.findWithDefault maxBound v info.dist 

dijkstra :: Graph -> State -> Info 
dijkstra graph source =
  let
    loop :: [State] -> Info -> Info 
    loop q info =
      case minViewOn (distanceOf info) q of
        Nothing ->
          -- we're done!
          info
        Just (u, q') ->
          let du = distanceOf info u
          in 
          if du < maxBound then             
            let
                update :: Info -> State -> Info 
                update i v =
                  let
                    alt = du + graph.cost u v
                  in
                    case compare alt (distanceOf i v) of
                      LT ->
                        MkInfo
                          (Map.insert v alt i.dist)
                          (Map.insert v [u] i.prev)
                      EQ ->
                        MkInfo
                          (Map.insert v alt i.dist)
                          (Map.insertWith (++) v [u] i.prev)
                      GT ->
                        i
                info' =
                  foldl' update info
                      (filter (`elem`q') (graph.neighbours u))
              in
                loop q' info'
            else
              -- nothing can happen anymore
              info

  in
    loop graph.vertices
      (MkInfo
        { dist = Map.singleton source 0
        , prev = Map.empty
        }
      )


---------------------------------------------------------------------
-- for part 2
pathsFrom :: Info -> State ->  [[Loc]]
pathsFrom info = go 
  where
    go s 
      = case Map.lookup s info.prev of
          Just ss -> [fst s:path | s'<-ss, path <- go s'  ]
          Nothing -> [[fst s]]







