{-# OPTIONS_GHC -Wall -Werror #-}

module Main where

import System.IO ( hPutStrLn, stderr, hPrint )
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )

import LexInstant
import ParInstant
import AbsInstant

import Flatten (compileLLVM)
import Stackify (compileJVM)
import CompilerErr (errorToString, CompilerErrorM)

import ErrM

type ParseFun a = [Token] -> Err a

getCompiler :: String -> IO (Program -> CompilerErrorM String)
getCompiler mode
  | mode == "llvm" = return compileLLVM
  | mode == "jvm" = return compileJVM
  | otherwise = do hPutStrLn stderr "wrong mode, should be llvm or jvm"
                   exitFailure

run :: String -> ParseFun Program -> String -> IO ()
run mode p s =
  do compiler <- getCompiler mode
     let ts = myLexer s in case p ts of
          Bad descr ->  do hPutStrLn stderr "\nParse              Failed...\n"
                           hPutStrLn stderr "Tokens:"
                           hPrint stderr ts
                           hPutStrLn stderr descr
                           exitFailure
          Ok  tree ->  case compiler tree of
            Left ce      -> do
                              hPutStrLn stderr (errorToString ce)
                              exitFailure
            Right output -> do
                              putStr output
                              exitSuccess

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
