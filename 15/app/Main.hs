{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
--
-- Day 15: Warehouse Woes ---
--
module Main where

import           System.Environment
import           Data.List
import           Data.Char
import           Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      input <- readFile path
      putStrLn ("Part1: " ++ show (part1 input))
      putStrLn ("Part2: " ++ show (part2 input))
    _ -> error "invalid arguments"

-- input types

type Loc = (Int,Int)

data Object = Wall | BoxS | BoxL | BoxR deriving (Eq, Show)
-- wall, small box, left half of large box, right half of large box

data Warehouse
  = Warehouse { objects :: Map Loc Object
              , robot :: Loc
              }
    deriving Show

data Dir = L | R | D | U deriving (Eq, Show)

type Input = String 

-- parser for objects and moves
parse :: String -> (Warehouse, [Dir])
parse txt
  = let ls = lines txt
        objs = parseObjects (takeWhile (not.null) ls)
        moves =  parseMoves (dropWhile (not.null) ls)
    in (objs, moves)

parseObjects :: [String] -> Warehouse
parseObjects xss
  = Warehouse objs robot
  where objs = Map.fromList [ ((i,j),toObj x) |
                              (i,xs) <- zip [0..] xss
                              , (j,x)<-zip [0..] xs
                              , x `elem`"#O[]"]
        robot = head [ (i,j) | (i,xs) <- zip [0..] xss
                             , (j,x)<-zip [0..] xs,
                              x =='@' ]
        toObj '#' = Wall
        toObj 'O' = BoxS
        toObj '[' = BoxL
        toObj ']' = BoxR
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
part1 txt = let (wh, moves) = parse txt
            in checkSum (foldl' moveRobot wh moves)

-- compute the GPS checksum
checkSum :: Warehouse -> Int
checkSum Warehouse{..} = Map.foldrWithKey' accum 0 objects
  where
  accum :: Loc -> Object -> Int -> Int
  accum (x,y) BoxS !total = total + 100*x+y
  accum (x,y) BoxL !total = total + 100*x+y
  accum _     _    !total = total


moveRobot :: Warehouse -> Dir -> Warehouse
moveRobot wh@Warehouse{..} dir
  = let robot' = direction dir robot
    in case push wh robot' dir of
      Just wh' -> wh' { robot = robot' }
      Nothing -> wh

-- move in a given direction
direction :: Dir -> Loc -> Loc
direction dir (x,y)
  = case dir of
      U -> (x-1,y)
      D -> (x+1,y)
      R -> (x,y+1)
      L -> (x,y-1)

-- recursively push object(s) in a location in a given direction
-- NB: this code is convoluted! Perhaps we could make it simpler?
push :: Warehouse -> Loc -> Dir  -> Maybe Warehouse
push wh loc dir 
  = case Map.lookup loc (objects wh) of
      Nothing ->
        Just wh

      Just Wall ->
        Nothing
        
      Just BoxS -> do
        let loc' = direction dir loc
        wh' <- push wh loc' dir
        return wh' { objects = Map.insert loc' BoxS $
                               Map.delete loc $
                               objects wh'
                   }
        
      Just BoxL -> 
        if dir == U || dir == D then
          do
            let loc' = direction dir loc
            let loc''= direction R loc'
            wh' <- push wh loc'' dir
            wh''<- push wh' loc' dir
            return wh'' { objects = Map.insert loc' BoxL $
                                    Map.insert loc'' BoxR $
                                    Map.delete loc $
                                    Map.delete (direction R loc) $
                                    objects wh''
                        }
        else
          do
            assert (dir == R) "push: BoxL violation"
            let loc' = direction R loc
            let loc'' = direction R loc'
            wh' <- push wh loc'' R
            return wh' { objects = Map.insert loc' BoxL $
                                   Map.insert loc'' BoxR $
                                   Map.delete loc $
                                   objects wh' }
          
        
          
      Just BoxR ->
        if dir==U || dir==D then
          do
            let loc' = direction dir loc
            let loc''= direction L loc'
            wh' <- push wh loc'' dir
            wh''<- push wh' loc' dir
            return wh'' { objects = Map.insert loc' BoxR $
                                    Map.insert loc'' BoxL $
                                    Map.delete loc $
                                    Map.delete (direction L loc) $
                                    objects wh''
                        }
        else
          do
            assert (dir == L) "push: BoxR violation"
            let loc' = direction L loc
            let loc'' = direction L loc'
            wh' <- push wh loc'' L
            return wh' { objects = Map.insert loc' BoxR $
                                   Map.insert loc'' BoxL $
                                   Map.delete loc $
                                   objects wh' }
            


---------------------------------------------------------------------
part2 :: Input -> Int
part2 txt = let (wh, moves) = parse (expand txt)
            in checkSum (foldl' moveRobot wh moves)
      
-- expand map representation (for part 2)
expand :: String -> String
expand = concatMap dup
  where
    dup x = case x of
      '#' -> "##"
      '.' -> ".."
      '@' -> "@."
      'O' -> "[]"
      ch ->  [ch]


-- debugging
assert :: Monad m => Bool -> String -> m ()
assert cond msg | cond = return ()
                | otherwise = error msg

        

