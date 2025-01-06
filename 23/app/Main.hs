--
-- Day 23: LAN Party ---
--
module Main where

import           System.Environment
import           Data.List 
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Function (on)

main :: IO ()     
main = do
  args <- getArgs
  case args of
    [path] -> do
      input <- readInput path
      putStrLn ("Part1: " ++ show (part1 input))
      putStrLn ("Part2: " ++ show (part2 input))
    _ -> error "invalid arguments"

type Input = [(String,String)]

readInput :: FilePath -> IO Input
readInput path = do
  txt <- readFile path
  return $ map parse (lines txt)
  where
    parse :: String -> (String,String)
    parse xs = case split '-' xs of
                 [a,b] -> if a<=b then (a,b) else (b,a)
                 _ -> error "invalid input"

split :: Char -> String -> [String]
split c xs
  | null xs = []
  | otherwise = takeWhile (/=c) xs : split c (drop 1 $ dropWhile (/=c) xs)

-------------------------------------------------------------------------
part1 :: Input -> Int
part1 edges
  = let verts = nub (map fst edges ++ map snd edges)
        edgeSet = Set.fromList edges
    in length (filter checkT (triples verts edgeSet))

checkT :: (String,String,String) -> Bool
checkT (x,y,z)
  = "t" `isPrefixOf` x || "t" `isPrefixOf` y || "t" `isPrefixOf` z

triples :: Ord v => [v] -> Set (v,v) -> [(v,v,v)]
triples verts edgeSet
  = [(x,y,z)
    | x:verts'<-tails verts, y:verts''<-tails verts', z<-verts'',
      (x,y) `member` edgeSet,
      (x,z) `member` edgeSet,
      (y,z) `member` edgeSet ]

member :: Ord a => (a,a) -> Set (a,a) -> Bool
member (x,y) set | x<=y = Set.member (x,y) set
                 | otherwise = Set.member (y,x) set

---------------------------------------------------------------------
part2 :: Input -> String
part2 edges
  = let verts = nub (map fst edges ++ map snd edges)
        edgeSet = Set.fromList edges
        clique = maximumBy (compare`on`length)
                 [maximalClique v (delete v verts) edgeSet | v<-verts]
    in concat (intersperse "," $ sort clique)


maximalClique :: Ord v => v -> [v] -> Set (v,v) -> [v]
maximalClique first rest edges
  = go [first] rest
  where
    go acc [] =  acc
    go acc (v:vs)
      | all (adjacent v) acc = go (v:acc) vs
      | otherwise            = go acc vs

    adjacent u v = (u,v) `member` edges
