--
-- Day 24: Crossed Wires ---
--
module Main where

import System.Environment
import Data.List
import Data.Bits

import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      input <- readInput path
      print input
      putStrLn ("Part1: " ++ show (part1 input))
      putStrLn ("Part2: " ++ show (part2 input))
    _ -> error "invalid arguments"

type Input = ([Initial], [Gate])

readInput :: FilePath -> IO Input
readInput path = do
  txt <- readFile path
  let ls = lines txt
  let ls' = takeWhile (not.null) ls
  let ls''= tail (dropWhile (not.null) ls)
  return (map parseWire ls', map parseGate ls'')

-----------------------------------------------------------
data Op = AND | OR | XOR deriving (Show, Read)

type Wire = String

type Initial = (Wire, Int)  -- 0/1

type Gate = (Wire, Op, Wire, Wire)   -- x op y -> z

-- parse a single line with an initial wire value
parseWire :: String -> Initial
parseWire txt
  = case words txt of
      [lhs, value] -> (init lhs, read value)
      _ -> error ("invalid wire: " ++ show txt)

-- parse a single line with a gate
parseGate :: String -> Gate
parseGate txt
  = case words txt of
      [x, op, y, "->", z] -> (x, read op, y, z)
      _ -> error ("invalid gate: " ++ show txt)

----------------------------------------------------------------
type Env = Map Wire Int

-- update the environment by running a gate (if applicable)
update :: Env -> Gate -> Env
update env (x,op,y,z)
  = case (Map.lookup x env, Map.lookup y env)  of
      (Just a, Just b) -> Map.insert z (apply op a b) env
      _ -> env

updateMany :: Env -> [Gate] -> Env
updateMany = foldl' update

apply :: Op -> Int -> Int -> Int
apply AND = (.&.)
apply OR  = (.|.)
apply XOR = xor


--------------------------------------------------------
part1 :: Input -> Int
part1 (initial, gates)
  = let final = go env0 (updateMany env0 gates)
    in toDecimal $ Map.elems $ Map.filterWithKey (\key _ -> "z" `isPrefixOf` key) final
  where
     env0 = Map.fromList initial
     go env env'
       | env' == env = env
       | otherwise = go env' (updateMany env' gates)
     toDecimal = foldl' (\acc d->d+acc*2) 0 . reverse


---------------------------------------------------------
part2 :: Input -> Int
part2 input = 0
