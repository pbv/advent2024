--
-- Day 19: Linen Layout ---
--
module Main where

import           System.Environment
import           Data.List
import           Data.Maybe
import qualified Data.Map as Map
import           Data.Map (Map)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      input <- readInput path
      putStrLn ("Part1: " ++ show (part1 input))
      putStrLn ("Part2: " ++ show (part2 input))
    _ -> error "invalid arguments"

--------------
type Input = ([String], [String])   -- wordlist, strings-to-match

readInput :: FilePath -> IO Input
readInput path = do
  txt <- readFile path
  return $ parse txt

parse :: String -> Input
parse txt
  = let (first:_:rest) = lines txt
    in (map (filter (/=',')) (words first), rest)

-- | a Trie structure for storing string prefixes
newtype Trie
  = MkTrie (Map Char Trie)
  deriving Show

emptyTrie :: Trie
emptyTrie = MkTrie Map.empty

-- | insert a word in the trie
insertTrie :: String -> Trie -> Trie
insertTrie [] (MkTrie branches)
  = MkTrie (Map.insert sentinel emptyTrie branches)
insertTrie (x:xs) (MkTrie branches)
  = MkTrie (Map.alter update x branches)
  where
    update Nothing      = Just (insertTrie xs emptyTrie)
    update (Just trie') = Just (insertTrie xs trie')

-- sentinel for word termination
sentinel :: Char
sentinel = '$'

-- | make a trie from a list of words
makeTrie :: [String] -> Trie
makeTrie = foldr insertTrie emptyTrie


-- | match the Kleene closure (*-closure) of words in a trie
matchStar :: Trie -> String  -> Bool
matchStar trie xs 
  = any isFinal (matchNFA trie xs [trie])

isFinal :: Trie -> Bool
isFinal (MkTrie branches) = sentinel `Map.member` branches

-- matching in a non-deterministic automaton;
-- the states are lists of tries
matchNFA :: Trie -> String -> [Trie] -> [Trie]
matchNFA start xs tries = go xs tries
  where
    go []     ts = ts
    go (x:xs) ts = let ts' = submatches x ts
                   in  go xs ([start | any isFinal ts'] ++ ts')

submatches ::  Char -> [Trie] -> [Trie]
submatches x tries
  =  catMaybes [Map.lookup x branches | MkTrie branches<-tries] 
             
--- debugging
-- build a regexp for the Kleene closure of the list of words
makeRegexp :: [String] -> String
makeRegexp list = "^(" ++ intercalate "|" list  ++ ")*$"
        

------------------------------------------------------
part1 :: Input -> Int
part1 (wordlist, strings) 
  = let trie = makeTrie wordlist
    in length (filter (matchStar trie) strings)

                 
part2 :: Input -> Int
part2 _input = 0 
