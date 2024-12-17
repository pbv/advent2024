--
-- Day 17: Chronospatial Computer ---
--
{-# LANGUAGE RecordWildCards #-}
module Main where

import System.Environment
import Data.List
import Data.Bits
import Data.Array

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

---------------------------------------
type Input = (Registers, Code)

data Registers
  = Registers { regIp :: !Int
              , regA :: !Int
              , regB :: !Int
              , regC :: !Int
              }
    deriving Show

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
  | 0<=opa && opa<=3 = opa
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
run regs0 code = loop regs0 (size+1)
  where
    size = length code 
    prog = listArray (0, size-1) code
    loop :: Registers -> Int -> Output
    loop regs@Registers{..} gas
      | regIp > size-1 = []
      | gas <= 0       = []
      | otherwise 
           = let opc = prog!regIp
                 opa = prog!(regIp+1)
                 (regs', out) = eval regs opc opa
             in case out of
                   Nothing -> loop regs' gas
                   Just v -> v : loop regs' (gas-1)
                

------------------------
part2 :: Input -> Int
part2 (regs,code)
  = head [a | a<-[0..], run regs{regA=a} code == code ]
