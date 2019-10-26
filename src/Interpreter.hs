module Interpreter where
import VM
import VM.Internal.MonadParser
import VM.InstructionSet
import VM.Serialization
import VM.Parser
--import VM.AssemblyParser
import System.IO(hFlush, stdin, stdout, hSetBuffering, BufferMode(..))
import System.Environment(getArgs)
import System.Process(system)
import Data.Array.Base
import Data.Maybe
import System.Directory(getCurrentDirectory)

import Data.Char(isSpace)
import Control.Monad.State

data Interpreter = Interpreter {
          vm :: VM
        , activeFile :: String
} deriving Show


hex' 0 _ = "0x"
hex' n m = hex' (n - 1) q ++ [chars !! fromIntegral r]
    where
        (q, r) = m `divMod` 16
        chars = "0123456789abcdef"

hex 0 _ = "0x0"
hex n m = hex' n m

address i = hex 8 (4 * i)

memoryText vm = (unlines $ map (\(i, e) -> address i ++ ": " ++ hex 8 e) (assocs (mem vm)))

opText vm = unlines . zipWith (joinBy ": ") addresses $ zipWith (joinBy " ") machineCodes asmMnemonics
    where
        isPc x = 4 * x == regs vm ! 15
        joinBy sep x y = x ++ sep ++ y
        addresses = map (\x -> (if isPc x then " > " else "   ") ++ address x) $ indices (text vm)
        machineCodes = map (const []) $ elems (text vm) {- machineCode -}
        asmMnemonics = map asmMnemonic $ elems (text vm)

registersText vm = unlines zipped
    where
        regName n = (map (\x -> "%r" ++ show x) [0..12] ++ ["sp", "lr", "pc"]) !! (fromIntegral n)
        regTexts = map (\(i, e) -> regName i ++ " = " ++ hex 8 e) $ assocs (regs vm)
        (col1, col2) = (take 8 regTexts, drop 8 regTexts)
        zipped = zipWith (\x y -> x ++ "\t" ++ y) col1 col2

loadFile :: String -> IO (ParseResult VM)
loadFile file = do
    (header:content) <- readFile file >>= (return . lines)
    let [memStart, memEnd] = map read $ words header
    let ops = parseProgram (unlines content)

    case ops of
        Result ops -> do
            let text = listArray (0, fromIntegral $ length ops - 1) ops
            let mem = listArray (memStart `div` 4, memEnd `div` 4) $ replicate (fromIntegral $ (memEnd - memStart) `div` 4) 0
            let regs = listArray (0, 15) $ replicate 16 0
            return (Result $ VM regs mem text)
        Failure msg -> return (Failure msg)

setUI :: MonadIO io => StateT Interpreter io ()
setUI = gets vm >>= \vm -> liftIO $ do
    system "tput reset"
    putStr "\ESC[3J"
    putStrLn "'r' for restart / ny innlasting av fil, CMD + . for å avslutte\n"
    putStrLn "Memory"
    putStrLn $ memoryText vm
    putStrLn ""
    putStrLn "Registers"
    putStrLn $ registersText vm
    putStrLn ""
    putStrLn "Instruction stream"
    putStrLn $ opText vm
    hFlush stdout

putVM :: Monad m => VM -> StateT Interpreter m ()
putVM vm' = do
    int <- get
    put $ int { vm = vm' }


attemptReload :: StateT Interpreter IO ()
attemptReload = do
    file <- gets activeFile
    content <- liftIO $ loadFile file

    case content of
        Result vm -> putVM vm
        Failure msg -> liftIO $ putStrLn msg >> getChar >> return ()


interpret :: StateT Interpreter IO VMExit
interpret = do
    setUI
    c <- liftIO getChar

    case c of
        '\n' -> do
            vm <- gets vm
            either return (putVM >=> const interpret) (step vm)
        'r' -> attemptReload >> interpret
        '\ESC' -> interpret --return UserExit -- having some problems with reenabling echoing
        _ -> interpret

-- | Fully evaluate a program until it exits, returning the current state and exit code
evaluate :: VM -> (VM, VMExit)
evaluate vm = either ((,) vm) evaluate (step vm)

main :: IO ()
main = do
    [fileName] <- getArgs
    (header:file) <- readFile fileName >>= (return . lines)
    let [memStart, memEnd] = map read $ words header
    let ops = case parseProgram (unlines file) of
            Just o -> o
            Nothing -> error "Could not load file"
    let text = listArray (0, fromIntegral $ length ops - 1) ops
    let mem = listArray (memStart `div` 4, memEnd `div` 4) $ replicate (fromIntegral $ (memEnd - memStart) `div` 4) 0
    let regs = listArray (0, 15) $ replicate 16 0
    let vm = VM regs mem text

    print (evaluate vm)

main' :: IO ()
main' = do
    putStr "Assembly file: "
    hFlush stdout
    -- | Hacky preprocessing to enable MacOS drag'n'drop for file paths
    fileName <- fmap (reverse . dropWhile isSpace . reverse . filter (/= '\\')) getLine
    (header:file) <- readFile fileName >>= (return . lines)
    let [memStart, memEnd] = map read $ words header
    let ops = case parseProgram (unlines file) of
            Just o -> o
            Nothing -> error "Could not load file"
    let text = listArray (0, fromIntegral $ length ops - 1) ops
    let mem = listArray (memStart `div` 4, memEnd `div` 4) $ replicate (fromIntegral $ (memEnd - memStart) `div` 4) 0
    let regs = listArray (0, 15) $ replicate 16 0
    let vm = VM regs mem text
    let int = Interpreter vm fileName
    -- | Disable echoing to the console
    system "stty -echo"
    -- | Resize the window to an appropriate size
    putStrLn "\ESC[8;36;80t"
    -- | Disable buffering, enabling us to read single chars from stdin
    hSetBuffering stdin NoBuffering

    -- | Interpret until the end.
    exit <- evalStateT (attemptReload >> interpret) int-- interpreter-- interpret vm

    -- | Re-enable echoing to the console
    system "stty echo"

    print exit
    return ()
