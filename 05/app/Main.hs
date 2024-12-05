--
-- Day 05: Printer Queue
--
module Main where
import           System.Environment
import           Data.List

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do --part1 path
                 part2 path
    _ -> error "invalid arguments"


type Order = [(Int,Int)]
type Update = [Int]

-- reading the input
data Input
  = Input { ordering :: Order
          , updates :: [Update]
          } deriving Show

readInput :: FilePath -> IO Input
readInput path = do
  ls <- lines <$> readFile path
  let (ords, ls') = readOrdering ls
  let ups = readUpdates ls'
  return Input {ordering = ords, updates=ups}

readOrdering :: [String] -> (Order, [String])
readOrdering ls
  = let pairs = map readPair $ takeWhile (not . null) ls
        _:rest = dropWhile (not . null) ls
    in (pairs, rest)

readPair :: String -> (Int,Int)
readPair txt
  = let a = read (takeWhile (/='|') txt)
        b = read (tail $ dropWhile (/='|') txt)
    in (a, b)

readUpdates :: [String] -> [Update]
readUpdates = map (read . braces)
  where braces x = "[" ++ x ++ "]"


--  part 1
part1 :: FilePath -> IO ()
part1 path = do
  input <- readInput path
  let valid = filter (checkUpdate (ordering input)) (updates input)
  print (sum $ map middle valid)

checkUpdate :: Order -> Update -> Bool
checkUpdate order pages = go pages
  where go (p:ps) = and [(p',p)`notElem`order | p'<-ps] && go ps
        go []     = True

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

--  part 2
part2 :: FilePath -> IO ()
part2 path = do
  input <- readInput path
  let invalid = filter (not . checkUpdate (ordering input)) (updates input)
  let fixed = map (reorder (ordering input)) invalid
  print (sum $ map middle fixed)


reorder :: Order -> Update -> Update
reorder order pages = go pages
  where
    go [] = []
    go (p:ps)
      = let ps' = [p' | p'<-ps, (p',p)`elem`order]
            ps''= ps \\ ps'
        in go ps' ++ p:go ps''



