--
-- Day 17: Chronospatial Computer ---
--
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import           System.Environment
import           Data.List
import           Data.Bits
import qualified Data.Vector.Unboxed as U
import           Data.Vector.Unboxed ((!))

import           Control.Monad
import           Control.Monad.State.Strict


main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      input <- readInput path
      putStrLn ("Part1: " ++ show (part1 input))
      part2 input
    _ -> error "invalid arguments"

---------------------------------------
type Input = (Registers, Code)

data Registers
  = Registers { regIp :: !Int
              , regA :: !Int
              , regB :: !Int
              , regC :: !Int
              }
    deriving (Show)

-- instance Hashable Registers

type Code = [Int]


readInput :: FilePath -> IO Input
readInput path = do
  ls <- lines <$> readFile path
  let (regs, ls') = parseRegs ls
  let (code, ls'') = parseCode (tail ls') -- skip blank line
  return (regs,code)

parseRegs :: [String] -> (Registers, [String])
parseRegs (txt1:txt2:txt3:rest)
  = let a = read (consume "Register A: " txt1)
        b = read (consume "Register B: " txt2)
        c = read (consume "Register C: " txt3)
    in (Registers { regIp = 0, regA = a, regB = b, regC = c },
        rest)

parseCode :: [String] -> (Code, [String])
parseCode (txt:rest)
  = let c = read ("[" ++ consume "Program: " txt ++ "]")
    in (c, rest)

consume :: String -> String -> String
consume prefix string
  | prefix `isPrefixOf` string = drop (length prefix) string
  | otherwise = error ("expecting " ++ show prefix)

-------------------------------------------------------------------------
part1 :: Input -> Output
part1 (regs,code) = run regs code

-- fetch the operand
combo :: Registers -> Int -> Int
combo Registers{..}  opa
  | 0<=opa && opa<=3= opa
  | opa == 4        = regA
  | opa == 5        = regB
  | opa == 6        = regC
  | otherwise       = error "combo: invalid operand"

-- perform an instruction
eval :: Registers -> Int -> Int -> (Registers, Maybe Int)
eval regs@Registers{..} opc opa
  | opc==0   -- adv
     = (regs { regA = regA `shiftR` (combo regs opa)
             , regIp = regIp + 2 }
       , Nothing)
  | opc==1   -- bxl
     = (regs { regB = regB `xor` opa
             , regIp = regIp + 2 }
       , Nothing)
  | opc==2   -- bst
     = (regs { regB = 0x7 .&. (combo regs opa)
             , regIp = regIp + 2 }
       , Nothing)
  | opc==3   -- jnz
      = if regA==0 then
               (regs { regIp = regIp + 2 }, Nothing)
          else (regs { regIp = opa }, Nothing)
  | opc==4   -- bxc
      = (regs { regB = regB `xor` regC
              , regIp = regIp + 2 }
        , Nothing)
  | opc==5   -- out
      = (regs { regIp = regIp + 2 }
        , Just ((combo regs opa) .&. 0x7))
  | opc==6   -- bdv
      = (regs { regB = regA `shiftR` (combo regs opa)
              , regIp = regIp + 2
              }, Nothing)
  | opc==7   -- cdv
      = (regs { regC = regA `shiftR` (combo regs opa)
              , regIp = regIp + 2
              }, Nothing)  
        
-- run a program until halting
type Output = [Int]

run :: Registers -> Code -> Output
run regs0 code = loop regs0 
  where
    size = length code 
    prog = U.fromList code
    loop :: Registers -> Output
    loop regs@Registers{..} 
      | regIp > size-1 = []
      | otherwise 
           = let opc = prog!regIp
                 opa = prog!(regIp+1)
                 (regs', out) = eval regs opc opa
             in case out of
                   Nothing -> loop regs' 
                   Just v -> v : loop regs' 
                

----------------------------------------------------------------------------
-- Part 2

-- Machinery for translation of programs into Z3 bit-vector constraints 
-- for solving with the Python Z3 bindings

type Symbolic a = State (Int,Int,Int,[Int]) a

emit :: [String] -> Symbolic String
emit = return . unwords

getA, getB, getC, getA', getB', getC', getO :: Symbolic String
-- get current registers
getA = get >>= \(c,_,_,_) -> return ("a" ++ show c)
getB = get >>= \(_,c,_,_) -> return ("b" ++ show c)
getC = get >>= \(_,_,c,_) -> return ("c" ++ show c)
-- increment and get next registers
getA' = modify' (\(a,b,c,o)->(a+1,b,c,o)) >> getA 
getB' = modify' (\(a,b,c,o)->(a,b+1,c,o)) >> getB
getC' = modify' (\(a,b,c,o)->(a,b,c+1,o)) >> getC

-- get next output
getO =
  do (a,b,c,xs) <- get 
     put (a,b,c,tail xs)
     return (show (head xs))

-- symbolic version of combo
scombo :: Int -> Symbolic String
scombo opa
  | 0<=opa && opa<=3 = pure (show opa)
  | opa == 4         = getA
  | opa == 5         = getB
  | opa == 6         = getC

-- symbolic translation of an instruction
seval :: Int -> Int -> Symbolic String
seval opc opa
  | opc == 0 = do
      t1 <- getA
      t2 <- scombo opa
      t3 <- getA'
      emit [t3, "==", t1, ">>", t2]
  | opc == 1 = do
      t1 <- getB
      t2 <- getB'
      emit [t2, "==", t1, "^", show opa]
  | opc == 2 = do
      t1 <- scombo opa
      t2 <- getB'
      emit [t2, "==", t1, "&", "7"]
  | opc == 3 = do
      t1 <- getA
      emit [t1, "!=", "0"]   -- assumes the jump is *always* taken!
  | opc == 4 = do
      t1 <- getB
      t2 <- getC
      t3 <- getB'
      emit [t3, "==", t1, "^", t2]
  | opc == 5 = do
      t1 <- scombo opa
      t2 <- getO
      emit [t1, "&", "7", "==", t2]
  | opc == 6 = do
      t1 <- getA
      t2 <- scombo opa
      t3 <- getB'
      emit [t3, "==", t1, ">>", t2]
  | opc == 7 = do
      t1 <- getA
      t2 <- scombo opa
      t3 <- getC'
      emit [t3, "==", t1, ">>", t2]
  | otherwise =
      error ("seval: " ++ show opc ++ " " ++ show opa)
      
-- | assert loop termination (for use at the end)
halt :: Symbolic String
halt = do
  t1 <- getA
  emit [t1, "==", "0"]

-- translate a program
-- only for "simple programs" (see definition later)
translate :: Code -> [String]
translate prog 
  | isSimpleLoop prog = header ++ decls ++ startup ++ constrs ++ footer
  | otherwise = error "unfolder: not a simple loop"
  where
    n = length prog
    prog' = initOps (concat (replicate n prog))
    (eqs, (a,b,c,_)) = runState (go prog') (0,0,0,prog)

    constrs = [ "s.add(" ++ eq ++ ")" | eq <- eqs]
    decls = declares "a" a ++ declares "b" b ++ declares "c" c
    header = ["from z3 import *"]
    footer = ["assert s.check() == sat", "m = s.model()", "print(m[a0])"]
    startup = ["s = Solver()", "s.add(b0 == 0)", "s.add(c0 == 0)"]
    -- declare symbolic variables
    declares :: String -> Int -> [String]
    declares prefix n = [v++ " = BitVec('"++ v ++"',64)"
                        | i<-[0..n], let v = prefix++show i]
    -- translate a code sequence into formulas
    go []  = do h <- halt; return [h]
    go (opc:opa:rest) = do
      eq <- seval opc opa
      eqs' <- go rest
      return (eq:eqs')


-- check for a "simple loop" program
-- only 1 jump at the end (back to instruction 0);
-- only 1 out instruction
isSimpleLoop :: Code -> Bool
isSimpleLoop code
  = lastOp code == (3,0) && countOp 3 code == 1 && countOp 5 code == 1

lastOp :: Code -> (Int,Int)
lastOp code = case reverse code of
                (opa:opc:_) -> (opc,opa)
                _ -> error "lastOp: invalid code"

initOps :: Code -> Code
initOps = reverse . drop 2 . reverse

countOp :: Int -> Code -> Int
countOp opc code
  = length [opc' | (opc',True)<-zip code (cycle [True,False]), opc'==opc]

---------------------------------------------------------------------
part2 :: Input -> IO ()
part2 (_,code) = do
  writeFile constraintsPath (unlines $ translate code)
  putStrLn ("Part 2: generated " ++ show constraintsPath ++
            " file; run with python-z3!")

constraintsPath :: FilePath
constraintsPath = "constraints.py"
