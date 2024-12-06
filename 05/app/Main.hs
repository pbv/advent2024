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
    [path] -> do
      input <- readInput path
      putStrLn ("Part1: " ++ show (part1 input))
      putStrLn ("Part2: " ++ show (part2 input))
    _ -> error "invalid arguments"


type Order = [(Int,Int)]
type Update = [Int]

-- reading the input
type Input = (Order, [Update])

readInput :: FilePath -> IO Input
readInput path = do
  ls <- lines <$> readFile path
  let (order, ls') = readOrdering ls
  let updates = readUpdates ls'
  return (order, updates)

readOrdering :: [String] -> (Order, [String])
readOrdering ls
  = let pairs = map readPair $ takeWhile (not . null) ls
        rest = tail $ dropWhile (not . null) ls
    in (pairs, rest)

readPair :: String -> (Int,Int)
readPair txt
  = read $ "(" ++ map (\x -> if x=='|' then ',' else x) txt ++ ")"

readUpdates :: [String] -> [Update]
readUpdates = map (read . braces)
  where braces x = "[" ++ x ++ "]"


--  part 1
part1 :: Input -> Int
part1 (order,updates)
  = let valid = filter (checkUpdate order) updates 
  in sum (map middle valid)

checkUpdate :: Order -> Update -> Bool
checkUpdate order pages = go pages
  where go (p:ps) = and [(p',p)`notElem`order | p'<-ps] && go ps
        go []     = True

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

--  part 2
part2 :: Input -> Int
part2 (order,updates)
  = let invalid = filter (not . checkUpdate order) updates 
        fixed = map (reorder order) invalid
    in sum (map middle fixed)

reorder :: Order -> Update -> Update
reorder order pages = go pages
  where
    go [] = []
    go (p:ps)
      = let ps' = [p' | p'<-ps, (p',p)`elem`order]
            ps''= ps \\ ps'
        in go ps' ++ p:go ps''



