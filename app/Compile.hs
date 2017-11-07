module Main where


import System.IO ( stdin, hGetContents, hPutStrLn, hPutStr, stderr, hPrint )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import LexInstant
import ParInstant
import SkelInstant
import PrintInstant
import AbsInstant

import Flatten (compileLLVM)
import Stackify (compileJVM)
import CompilerErr (errorToString)

import ErrM

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

getCompiler mode
  | mode == "llvm" = compileLLVM
  | mode == "jvm" = compileJVM

run :: String -> ParseFun Program -> String -> IO ()
run mode p s = let ts = myLLexer s in case p ts of
          Bad s    -> do hPutStrLn stderr "\nParse              Failed...\n"
                         hPutStrLn stderr "Tokens:"
                         hPrint stderr ts
                         hPutStrLn stderr s
                         exitFailure
          Ok  tree ->  either (\ce ->
                           do
                            hPutStrLn stderr (errorToString ce)
                            exitFailure)
                          (\ output ->
                           do
                            putStr output
                            exitSuccess)
                          (getCompiler mode tree)


usage :: IO ()
usage = do
  hPutStrLn stderr $ unlines
    [ "usage: Call with one of the following arguments:"
    , "  --help          Display this help message."
    , "  llvm|jvm        Compile stdin."
    , "  llvm|jvm file   Compile content of file."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [mode] -> getContents >>= run mode pProgram
    [mode, file] -> readFile file >>= run mode pProgram
    _ -> usage
