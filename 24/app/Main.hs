--
-- Day 24: Crossed Wires ---
--
module Main where

import System.Environment
import Data.List
import Data.Bits
import Test.QuickCheck hiding ((.&.))
import Data.Graph (SCC(..), stronglyConnComp)
import Data.Function (on)

import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      input@(_,gates) <- readInput path
      putStrLn ("Part1: " ++ show (part1 input))
      let pairs = part2 input
      putStrLn ("Part2: " ++ show pairs)
      putStrLn "Checking solution..."
      checkSolution pairs gates
    _ -> error "invalid arguments"

type Input = ([Init], [Gate])

readInput :: FilePath -> IO Input
readInput path = do
  txt <- readFile path
  let ls = lines txt
  let ls' = takeWhile (not.null) ls
  let ls''= tail (dropWhile (not.null) ls)
  return (map parseInit ls', map parseGate ls'')

-----------------------------------------------------------
data Op = AND | OR | XOR deriving (Show, Read)

type Wire = String

type Init = (Wire, Int)  -- 0/1

type Gate = (Wire, Op, Wire, Wire)   -- x op y -> z

-- parse a single line with an initial wire value
parseInit :: String -> Init
parseInit txt
  = case words txt of
      [lhs, value] -> (init lhs, read value)
      _ -> error ("invalid line: " ++ show txt)

-- parse a single line with a gate
parseGate :: String -> Gate
parseGate txt
  = case words txt of
      [x, op, y, "->", z] -> (x, read op, y, z)
      _ -> error ("invalid line: " ++ show txt)


-- Just a topologically sort a list of gates;
-- or Nothing if the network has cycles
topoSort :: [Gate] -> Maybe [Gate]
topoSort gates = loop components
  where components = stronglyConnComp [(g, z, [x,y]) | g@(x,_,y,z)<-gates]
        -- loop over components gathering acyclic nodes or failing
        loop [] = Just []
        loop (AcyclicSCC v : rest) = (v:) <$> loop rest
        loop (CyclicSCC _ : rest) = Nothing

----------------------------------------------------------------
-- an enviroments maps wires to bit values
type Env = Map Wire Int

-- make an environment 
toEnv :: String -> Int -> Env
toEnv prefix value
  = let wires = map (showWire prefix) [0..]
        -- infinite binary stream from LSB -> MSB
        toBinary :: Int -> [Int]
        toBinary n = (n`mod`2) : toBinary (n`div`2)
    in Map.fromAscList $ take wordSize $ zip wires (toBinary value)

-- the number of bits for inputs and output
wordSize :: Int
wordSize = 45

-- show wire padding bit number with leading zeros
showWire :: String -> Int -> String
showWire prefix num
  | num < 10 = prefix ++ ('0' : show num)
  | otherwise = prefix ++ show num


-- get a integer value from the environment
-- NB: depends on the map ordering to process bits from to MSB -> LSB
fromEnv :: String -> Env -> Int
fromEnv prefix env
  = Map.foldrWithKey' (\key bit acc->if prefix`isPrefixOf`key
                                     then bit+acc*2 else acc) 0 env

-- update the environment by running a gate (if applicable)
update :: Env -> Gate -> Env
update env (x,op,y,z)
  = case (Map.lookup x env, Map.lookup y env)  of
      (Just a, Just b) -> Map.insert z (apply op a b) env
      _ -> error "invalid update"

compute :: Env -> [Gate] -> Env
compute = foldl' update

apply :: Op -> Int -> Int -> Int
apply AND = (.&.)
apply OR  = (.|.)
apply XOR = xor


--------------------------------------------------------
part1 :: Input -> Int
part1 (initial, gates)
  = let Just gates' = topoSort gates
        env = compute (Map.fromList initial) gates'
    in fromEnv "z" env

---------------------------------------------------------
part2 :: Input -> [(Wire,Wire)]
part2 (_, gates) =
  let Just gates' = topoSort gates
      solution = solve gates'
  in solution
  -- in concat $ intersperse "," $ sort $ concatMap (\(x,y) -> [x,y]) solution


solve :: [Gate] -> [(Wire,Wire)]
solve gates = go 0 wires gates
  where
    wires = sortWires [z | (_,_,_,z)<-gates]
    go b _ _ | b>=wordSize = []
    go b wires gates
      | checkBit b gates = go (b+1) wires gates
      | otherwise =
        -- found a faulty bit; try to resolve it
        let ((x,y),gates') = fixBit b wires gates
        in (x,y) : go (b+1) (wires\\[x,y]) gates'
        

fixBit :: Int -> [Wire] -> [Gate] -> ((Wire,Wire), [Gate])
fixBit b wires gates = go (pairs wires)
  where
  go [] = error ("could not fix bit " ++ show b)
  go (pair:rest) | Just gates'<-topoSort (switch pair gates),
                   checkBit b gates' = (pair, gates')
                 | otherwise = go rest
  

-- switch wires arounds
switch :: (Wire, Wire) -> [Gate] -> [Gate]
switch (w,w') gates
  =  [ (x,op,y,rename z) | (x,op,y,z) <- gates]
  where
    rename :: String -> String
    rename v | v==w = w'
             | v==w'= w
             | otherwise = v

-- check bit n of the adder
checkBit :: Int -> [Gate] -> Bool
checkBit n gates
  = and (map (\(x,y) ->
                    let env = Map.union (toEnv "x" x) (toEnv "y" y)
                        z = fromEnv "z" (compute env gates)
                    in (x+y).&.mask == z.&.mask)
              (testCases n))
  where
    mask = 2^n - 1

testCases :: Int -> [(Int,Int)]
testCases 0 = [(0,1), (1,0)]
testCases n | n>0 = [ (0, 2^n-1), (1, 2^n-1), (2^(n-1), 2^(n-1)), (0, 2^(n-1)) ]
            

-- pairs of distinct values from a list ignoring order
pairs :: [a] -> [(a,a)]
pairs xs = [(x,y) | x:ys<-tails xs, y<-ys]


-- sort wires so that outputs come last
sortWires :: [Wire] -> [Wire]
sortWires = sortBy cmp
  where cmp ('z':x) ('z':y) = compare x y
        cmp ('z':x) y = GT
        cmp x ('z':y) = LT
        cmp x y       = compare x y  

-- test the solution using QuickCheck 
--
checkSolution :: [(Wire,Wire)] -> [Gate] -> IO ()
checkSolution pairs gates
  = let gates'= foldl (flip switch) gates pairs
    in case topoSort gates' of
         Nothing -> error "invalid solution: cyclic network"
         Just gates'' ->
           quickCheckWith stdArgs{maxSize=100000,maxSuccess=10000}
           (prop_correct_adder gates'')
        
prop_correct_adder :: [Gate] -> Property
prop_correct_adder gates
  = forAll arbitrarySizedNatural $ \x ->
    forAll arbitrarySizedNatural $ \y ->
    let env = Map.union (toEnv "x" x) (toEnv "y" y)
    in fromEnv "z" (compute env gates) == x+y

