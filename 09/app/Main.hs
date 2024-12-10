--
-- Day 9: Disk Fragmenter
--
{-# LANGUAGE BangPatterns #-}

module Main where

import           System.Environment
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq(..))
import           Data.Foldable (toList)

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
-- blocks of files or free space

data Block 
  = Free !Size 
  | File !Size !FileID !Bool  -- True if movable, False otherwise (for part 2)
  deriving Show

type Size = Int
type FileID = Int

type Input = Seq Block

readInput :: FilePath -> IO Input
readInput path = do
  ls <- lines <$> readFile path
  return $ case ls of
    [txt] -> parseBlocks txt
    _ -> error "invalid input"

parseBlocks :: String -> Seq Block
parseBlocks txt 
  = Seq.fromList $ parseAcc 0 (map (\x -> read [x]) txt) 
  where
     --
     parseAcc :: Int -> [Int] -> [Block]
     parseAcc !fid (size:size':sizes)
       | size'>0 = File size fid True : Free size' : parseAcc (fid+1) sizes
       | otherwise = File size fid True : parseAcc (fid+1) sizes
     parseAcc !fid [size] = [File size fid True]
     parseAcc _ [] = []


-- debugging
-- only works with single digit ids
unparseBlocks :: Seq Block -> String
unparseBlocks  = concatMap toString . toList
  where
    toString b
      = case b of
          Free size -> replicate size '.'
          File size fid _ -> replicate size (head $ show fid)


-------------------------------------------------------------------------
part1 :: Input -> Int
part1 = checkSum . compact1 

checkSum :: Seq Block -> Int
checkSum blocks = go 0 0 (toList blocks)
  where
    go _ acc [] = acc
    go !start !acc (b : bs)
      = case b of
          File size fid _ ->
            let c = sum [k*fid | k<-[start .. start+size-1]]
            in go (start+size) (acc+c) bs
          Free size ->
            go (start+size) acc bs



-- move files from the end to the beginning,
-- possibly splitting them up
-- NB: this part ignores the moveable flag
compact1 :: Seq Block -> Seq Block

compact1 (File size fid _ :<| bs)
  = File size fid False :<| compact1 bs

compact1 (Free size :<| (bs :|> File size' fid _))
  | size' <= size
  = File size' fid False :<| compact1 (Free (size-size') :<| bs)
  | otherwise  -- split up the file
  = File size fid False :<| compact1 (bs :|> File (size'-size) fid True)

compact1 (Free size :<| (bs :|> Free size'))
  = compact1 (Free size :<| bs) :|> Free size'

compact1 (b :<| Empty) = b :<| Empty
compact1 Empty = Empty




             
--------------------------------------------------------------------------
part2 :: Input -> Int
part2 = checkSum . compact2 

compact2 :: Seq Block -> Seq Block
compact2 (bs :|> File size fid False)   -- skip previously moved files
  = compact2 bs :|> File size fid False
compact2 (bs :|> File size fid True)    -- try to move a file
  = case firstFit size fid bs of
      Just bs' -> compact2 bs' :|> Free size
      Nothing -> compact2 bs :|> File size fid False
  
compact2 (bs :|> Free size) = compact2 bs :|> Free size
compact2 Empty = Empty

-- find the first complete fit for a movable file
firstFit :: Size -> FileID -> Seq Block -> Maybe (Seq Block)
firstFit size fid blocks = go blocks
  where
    go Empty = Nothing
    go (Free size' :<| bs)
        | size'>size  = Just (File size fid False :<| Free (size'-size) :<| bs)
        | size'==size = Just (File size fid False :<| bs)
        | otherwise   =  (Free size' :<|) <$> go bs
    go (b :<| bs)     = (b:<|) <$> go bs

-----------------------------------------------------------------
-- benchmarking

-- naive solution
--  Total   time    6.750s  (  6.750s elapsed)

-- distinguish moveable/non-moveable files
--  Total   time    5.143s  (  5.140s elapsed)

-- avoid creation of 0-size free blocks
--  Total   time    3.263s  (  3.260s elapsed)

