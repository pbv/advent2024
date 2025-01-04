--
-- Day 21: Keypad Conundrum
--
module Main where

import           System.Environment
import           Data.List
import           Data.Char

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      input <- readInput path
      putStrLn ("Part1: " ++ show (part1 input))
      -- too slow
      -- putStrLn ("Part2: " ++ show (part2 input))
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

type Pos = (Int,Int)  -- row, column
type Vec = (Int,Int)

findPos :: Char -> Keypad -> Pos
findPos c kp
  = head [(i,j) | (i,row)<-zip [0..] kp, (j,x)<-zip [0..] row, x == c]

-- translate keypresses on a keypad into keypresses on the directional keypad
translate :: Keypad -> String -> [String]
translate kp seq = go start seq 
  where start = findPos 'A' kp
        avoid = findPos '.' kp
        go loc [] = [""]
        go loc (c:cs) 
          = let loc' = findPos c kp
                delta= posDiff loc loc'               
            in [ m ++ ('A':ms) | m<-moves delta
                               , checkMoves avoid loc m 
                               , ms<-go loc' cs ]


moves :: Vec -> [String]
moves (di,dj)
  = nub [ vert di ++ horiz dj, horiz dj ++ vert di ]
  where
    vert d | d>=0 = replicate d 'v'
           | otherwise = replicate (-d) '^'
    horiz d | d>=0 = replicate d '>'
            | otherwise = replicate (-d) '<'

checkMoves :: Pos -> Pos -> String -> Bool
checkMoves avoid loc cs
  = go loc cs
  where go loc _ | loc == avoid = False
        go _ [] = True
        go (i,j) (c:cs)
          = case c of
              'v' -> go (i+1,j) cs
              '^' -> go (i-1,j) cs
              '>' -> go (i,j+1) cs
              '<' -> go (i,j-1) cs
        


posDiff :: Pos -> Pos -> Vec
posDiff (i,j) (i', j') = (i'-i, j'-j)


-- helper functions
translateNum = translate numKeypad
translateDir = translate dirKeypad

-- complexity of a sequence
complexity :: String -> Int
complexity seq = minLen seq * numeric seq


minLen :: String -> Int
minLen seq
  = minimum $ map length (translateNum seq >>= translateDir >>= translateDir)


numeric :: String -> Int
numeric seq = read (filter isDigit seq)
               
-------------------------------------------------------------
part1 :: Input -> Int
part1 input = sum (map complexity input)


-------------------------------------------------------------             
part2 :: Input -> Int
part2 input = sum (map complexity2 input)

-- complexity of a sequence
complexity2 :: String -> Int
complexity2 seq = minLen2 seq * numeric seq

minLen2 :: String -> Int
minLen2 seq
  = minimum $ map length (translateNum seq >>= iterM 25 translateDir)

iterM :: Monad m => Int -> (a -> m a) -> a -> m a
iterM n f a = go n a
  where
    go 0 x = pure x
    go k x = f x >>= go (k-1) 

