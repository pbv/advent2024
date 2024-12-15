{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
--
-- Day 15: Warehouse Woes ---
--
module Main where

import System.Environment
import Data.List
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map

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

data Object = Box | Wall deriving (Eq, Show)

data Warehouse
  = Warehouse { objects :: Map Loc Object
              , robot :: Loc
              }
    deriving Show

data Dir = L | R | D | U deriving Show

type Input = (Warehouse, [Dir])

readInput :: FilePath -> IO Input
readInput path = do
  ls <- lines <$> readFile path
  let objs = parseObjects (takeWhile (not.null) ls)
  let moves =  parseMoves (dropWhile (not.null) ls)
  return (objs, moves)

parseObjects :: [String] -> Warehouse
parseObjects xss
  = Warehouse objs robot
  where objs = Map.fromList [ ((i,j),toObj x) |
                              (i,xs) <- zip [0..] xss
                              , (j,x)<-zip [0..] xs
                              , x =='#' || x =='O' ]
        robot = head [ (i,j) | (i,xs) <- zip [0..] xss
                             , (j,x)<-zip [0..] xs,
                              x =='@' ]
        toObj '#' = Wall
        toObj 'O' = Box
        toObj _ = error "invalid object"

parseMoves :: [String] -> [Dir]
parseMoves = concatMap (map parseMove.(filter (not.isSpace)))
  where parseMove '>' = R
        parseMove '<' = L
        parseMove '^' = U
        parseMove 'v' = D
        parseMove _ = error "invalid move"


----------------------------------------------------------------------
part1 :: Input -> Int
part1 (wh, moves)
  = checkSum (foldl' move wh moves)

checkSum :: Warehouse -> Int
checkSum Warehouse{..} = Map.foldrWithKey' accum 0 objects
  where
  accum :: Loc -> Object -> Int -> Int
  accum (x,y) Box total  = total + 100*x+y
  accum _     Wall total = total


-- move the robot towards one direction, pushing boxes as needed
-- assumes the warehouse is walled all around
move :: Warehouse -> Dir ->  Warehouse
move wh@Warehouse{..} dir 
  = go (direction dir robot)
  where go :: [Loc] -> Warehouse
        go []
          = error "go: empty locations"
        go (loc:locs)
           = case Map.lookup loc objects of
               Nothing ->   -- a free location
                 wh { robot = loc }
               Just Box ->  -- a box
                 push loc locs
               Just Wall -> -- a wall
                 wh
        --
        push :: Loc -> [Loc] -> Warehouse
        push start [] 
          = error  "push: empty locations"
        push start (loc:locs)
          = case Map.lookup loc objects of
              Nothing ->  -- perform the push
                wh { robot = start,
                     objects = Map.insert loc Box (Map.delete start objects)
                   }
              Just Box ->
                push start locs
              Just Wall -> -- nothing gets pushed
                wh

-- all locations in a given direction
direction :: Dir -> Loc -> [Loc]
direction dir (x,y)
  = case dir of
      U -> [(x-i,y) | i<-[1..]]
      D -> [(x+i,y) | i<-[1..]]
      R -> [(x,y+i) | i<-[1..]]
      L -> [(x,y-i) | i<-[1..]]


---------------------------------------------------------------------
part2 :: Input -> Int
part2 input = 0
