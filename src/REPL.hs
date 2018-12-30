import VM
import VM.InstructionSet
import VM.Serialization
import VM.Internal.Parser
import VM.Parser
import System.IO(hFlush, stdin, stdout, hSetBuffering, BufferMode(..))
import System.Environment(getArgs)
import System.Process(system)
import Data.Array.Base
import Data.Maybe
import System.Directory(getCurrentDirectory)

import Data.Char(isSpace)
import Control.Monad.State


hex' 0 _ = "0x"
hex' n m = hex' (n - 1) q ++ [chars !! fromIntegral r]
    where
        (q, r) = m `divMod` 16
        chars = "0123456789abcdef"

hex 0 _ = "0x0"
hex n m = hex' n m

address i = hex 8 (4 * i)

memoryText vm = (unlines $ map (\(i, e) -> address i ++ ": " ++ hex 8 e) (assocs (mem vm)))

registersText vm = unlines zipped
    where
        regName n = (map (\x -> "%r" ++ show x) [0..3]) !! (fromIntegral n)
        regTexts = map (\(i, e) -> regName i ++ " = " ++ hex 8 e) $ assocs (regs vm)
        (col1, col2) = (take 2 regTexts, drop 2 regTexts)
        zipped = zipWith (\x y -> x ++ "\t" ++ y) col1 col2
setUI vm = do
    system "tput reset"
    putStr "\ESC[3J"
    putStrLn "Available instructions (R = register, I = immediate):"
    putStrLn "add R, R, R    ; dst <- lhs + rhs"
    putStrLn "sub R, R, R    ; dst <- lhs - rhs"
    putStrLn "mul R, R, R    ; dst <- lhs * rhs"
    putStrLn "cmp R, R, R    ; set bits [0..2] based on <, >, ="
    putStrLn "load R, R      ; dst <- memory pointed to be src"
    putStrLn "store R, R     ; val -> memory pointed to by dst"
    putStrLn "ldi R, I       ; dst <- immediate"
    putStrLn ""
    putStrLn "Memory"
    putStrLn $ memoryText vm
    putStrLn ""
    putStrLn "Registers"
    putStrLn $ registersText vm
    hFlush stdout

{-
ADD Reg Reg Reg
        | SUB Reg Reg Reg
        | MUL Reg Reg Reg
        | CMP Reg Reg Reg
        | JGT Reg Imm
        | LDI Reg Imm
        | LOAD Reg Reg
        | STORE Reg Reg
-}

replVM = vm
    where
        memStart = 0xF0
        memEnd = 0x104
        mem = listArray (memStart `div` 4, memEnd `div` 4) $ replicate (fromIntegral $ (memEnd - memStart) `div` 4) 0
        regs = listArray (0, 3) $ replicate 4 0
        vm = VM regs mem (error "repl should not use text")


repl vm = do
    setUI vm
    putStr "REPL> "
    hFlush stdout
    input <- getLine
    case runParser opParser input of
        Failure msg -> putStr msg >> hFlush stdout >> getChar >> repl vm
        Result (op, "") -> either (print >=> const (getChar >> repl vm)) repl (execute op vm)
        Result (op, remaining) -> putStr ("Could not parse entire instruction: " ++ remaining) >> hFlush stdout >> getChar >> repl vm

main = do
    putStrLn "\ESC[8;25;80t"
    repl (replVM)
    return ()
