module Main where

import           System.Environment
import           Data.List
import qualified Data.Map as Map

type Input = [[Char]]

readInput :: FilePath -> IO Input
readInput path = do
  txt <- readFile path
  return (lines txt)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      input <- readInput path
      putStrLn ("Part 1: " ++ show (part1 input))
      putStrLn ("Part 2: " ++ show (part2 input))
    _ -> error "invalid arguments"

-------------------------------------------------------------
-- part 1

part1 :: Input -> Int
part1 xss
  = horizontal xss + horizontal xss' + horizontal yss + horizontal yss'
  where xss' = transpose xss
        yss  = diagonals xss
        yss' = diagonals' xss

horizontal :: Input -> Int
horizontal xss
  = sum (map countLine xss) + sum (map (countLine.reverse) xss)

diagonals :: Input -> Input
diagonals xss = map snd $ Map.toList tabl
  where tabl = Map.fromListWith (++) [(i-j, [x])
                                     | (i,xs)<-zip [0::Int ..] xss,
                                       (j,x) <-zip [0::Int ..] xs ]

diagonals' :: Input -> Input
diagonals' xss = map snd $ Map.toList tabl
  where tabl = Map.fromListWith (++) [(i+j, [x])
                                     | (i,xs)<-zip [0::Int ..] xss,
                                       (j,x) <-zip [0::Int ..] xs ]

countLine :: [Char] -> Int
countLine xs
  | null xs              = 0
  | "XMAS" `isPrefixOf` xs = 1 + countLine (drop 4 xs)
  | otherwise            = countLine (tail xs) 


--------------------------------------------------
-- part 2
part2 :: Input -> Int
part2 xss
  = (countCrosses xss +  countCrosses (reverse xss) + 
    countCrosses xss' + countCrosses (reverse xss')) `div` 2
  where xss' = transpose xss


countCrosses :: Input -> Int
countCrosses xss =
  sum [ countCross r1 r2 r3
      | (r1,r2,r3) <- zip3 xss (tail xss) (tail $ tail xss)]


countCross :: String -> String -> String -> Int
countCross row1 row2 row3
  | null row1 || null row2 || null row3 = 0
  | otherwise
  = fromEnum c1 + fromEnum c2 +
    countCross (tail row1) (tail row2) (tail row3)
  where c1 = match "M.S" row1 && match ".A." row2 && match "M.S" row3
        c2 = match "S.M" row1 && match ".A." row2 && match "S.M" row3
  
match :: String -> String -> Bool
match ('.':ps) (_:xs)
  = match ps xs
match (p:ps)   (x:xs)
  | p == x = match ps xs
  | otherwise = False
match [] _ = True
match _ [] = False
