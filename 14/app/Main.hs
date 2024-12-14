--
-- Day 14: Restroom Redoubt ---
--
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import           System.Environment
import           Control.Monad
import qualified Data.IntMap as IntMap

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      input <- readInput path
      putStrLn ("Part1: " ++ show (part1 input))
      let k = part2 input
      let tree = map (forwards k) input
      putStrLn ("Part2: " ++ show k)
      printRobots tree
    _ -> error "invalid arguments"

--- input types
type Loc = (Int, Int)
type Vel = (Int, Int)
data Robot = Robot { position :: !Loc
                   , velocity :: !Vel
                   }
           deriving Show

type Input = [Robot]

readInput :: FilePath -> IO Input
readInput path = parse <$> readFile path

parse :: String -> Input
parse txt = map parseBot (lines txt)

parseBot :: String -> Robot
parseBot txt = let [pos,vel] = words txt
                   [_, p] = split '=' pos
                   [_, v] = split '=' vel
               in Robot (read ("("++p++")")) (read ("("++v++")"))
                         
split :: Char -> String -> [String]
split _ [] = []
split c xs = takeWhile (/=c) xs : split c (drop 1 $ dropWhile (/=c) xs)

------------------------------------------------------------------
part1 :: Input -> Int
part1 input = safety (map (forwards 100) input)

-- | move a robot n seconds forward
forwards :: Int -> Robot -> Robot
forwards n r@Robot{..}  = r { position = (x',y') }
  where
    (x,y) = position
    (vx,vy) = velocity
    !x' = (x + n*vx) `mod` width
    !y' = (y + n*vy) `mod` height


width, height :: Int
width =  101
height = 103

safety :: Input -> Int
safety input = q1 * q2 * q3 * q4
  where   
    q1 = length (filter quadrant1 (map position input))
    q2 = length (filter quadrant2 (map position input))
    q3 = length (filter quadrant3 (map position input))
    q4 = length (filter quadrant4 (map position input))    
    
quadrant1 :: Loc -> Bool
quadrant1 (x,y) = y<height`div`2 && x<width`div`2

quadrant2 :: Loc -> Bool
quadrant2 (x,y) = y<height`div`2 && x>width`div`2

quadrant3 :: Loc -> Bool
quadrant3 (x,y) = y>height`div`2 && x<width`div`2

quadrant4 :: Loc -> Bool
quadrant4 (x,y) = y>height`div`2 && x>width`div`2


-----------------------------------------------------------------
-- this is a bit of hack: find the smallest
-- number of steps that yield 20 or more robots
-- in horizontal and vertical line
part2 :: Input -> Int
part2 input
  = head [n | n<-[1..width*height],
          let out = map (forwards n) input,
          axis fst out >= 20, axis snd out >= 20   
          ]


-- count maximum robots on horizontal / vertical lines
axis :: (Loc -> Int) -> Input -> Int
axis proj input 
  = maximum $
    IntMap.fromListWith (+) $
    zip (map (proj.position) input) (repeat 1)

-- debugging
printRobots :: Input -> IO ()
printRobots robots = do
  forM_ [0..height-1] $ \y -> do
    forM_ [0..width-1] $ \x ->
       if (x,y) `elem` map position robots then
         putChar 'X'
         else
         putChar '.'
    putChar '\n'

