--
-- Day 06: Guard Gallivant
--
{-# LANGUAGE RecordWildCards #-}

module Main where

import           System.Environment
import           Data.Set (Set)
import qualified Data.Set as Set
import           Control.Monad hiding (guard)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      input <- readInput path
      putStrLn ("Part1: " ++ show (part1 input))
      putStrLn ("Part2: " ++ show (part2 input))
    _ -> error "invalid arguments"


-- the input types

type Loc = (Int,Int)

data Grid = Grid { width :: Int
                 , height :: Int
                 , obstacles :: Set Loc
                 }
            deriving Show

type Input = (Grid, Guard)

data Guard = Guard { location :: Loc
                   , direction :: Dir
                   }
           deriving Show

data Dir = L | R | U | D
         deriving (Eq, Ord, Show)

readInput :: FilePath -> IO Input
readInput path = do
  ls <- lines <$> readFile path
  let h = length ls
  let w = length (ls!!0) 
  let obstacles = Set.fromList [ (i,j)  | (i,l)<-zip [0..] ls,
                                 (j,x)<-zip [0..] l,  x=='#' ]
  let guard = head [ Guard (i,j) (parseDir x) 
                   | (i,l)<-zip [0..] ls,
                     (j,x)<-zip [0..] l,  x`elem`"<>^v" ]
  return (Grid w h obstacles, guard)

parseDir :: Char -> Dir
parseDir c
  = case c of
       '<' -> L
       '>' -> R
       '^' -> U
       'v' -> D
       _   -> error "invalid direction"

-------------------------------------------------------------------
-- debugging
-------------------------------------------------------------------
printInput :: Input -> IO ()
printInput (Grid{..}, Guard{..})
  = forM_ [0..height-1] $
    \i -> do forM_ [0..width-1]
               $ \j -> let c | (i,j) `Set.member` obstacles = '#'
                             | (i,j) == location  = unparseDir direction
                             | otherwise = '.'
                       in putChar c
             putChar '\n'

unparseDir :: Dir -> Char
unparseDir dir
  = case dir of
      U -> '^'
      D -> 'v'
      L -> '<'
      R -> '>'


---------------------------------------------------------------
part1 :: Input -> Int
part1 input = Set.size (walk input) 
  
walk :: Input -> Set Loc
walk (grid@Grid{..}, Guard{..})
  = go location direction (nextLoc location direction) (Set.singleton location)
  where
    go loc dir loc' visited
      | loc' `Set.member` obstacles = let dir' = rotate dir
                                          loc'' = nextLoc loc dir'
                                      in go loc dir' loc'' visited
      | loc' `inside` grid
               = go loc' dir (nextLoc loc' dir) (Set.insert loc' visited)
      | otherwise = visited

                                  
nextLoc :: Loc -> Dir -> Loc
nextLoc (x,y) dir 
  = case dir of
      U -> (x-1,y)
      D -> (x+1,y)
      R -> (x,y+1)
      L -> (x,y-1)

rotate :: Dir -> Dir
rotate dir
  = case dir of
      U -> R
      R -> D
      D -> L
      L -> U

inside :: Loc -> Grid -> Bool
inside (x,y) Grid{..}
  = x>=0 && y>=0 && x<width && y<height

--------------------------------------------------------------
part2 :: Input -> Int
part2 (grid@Grid{..}, guard@Guard{..})
  = let visited = walk (grid,guard)
        positions =  [pos | pos <- Set.toList visited, pos /= location]
    in length [p | p<-positions,
               let grid' = grid { obstacles = Set.insert p obstacles },
               checkLoop grid' guard ]
  

checkLoop :: Grid -> Guard -> Bool
checkLoop grid@Grid{..} Guard{..}
  = let location' = nextLoc location direction
        origin = Set.singleton (location,direction)
    in go location direction location' origin
  where
    go :: Loc -> Dir -> Loc -> Set (Loc,Dir) -> Bool
    go loc dir loc' acc
      | loc' `Set.member` obstacles = let dir' = rotate dir
                                      in go loc dir' (nextLoc loc dir') acc
      | loc' `inside` grid =
          if (loc',dir) `Set.member` acc then   -- stuck in a loop
            True
          else
            let acc' = Set.insert (loc',dir) acc
            in go loc' dir (nextLoc loc' dir) acc'
      | otherwise = False  -- got out

