{-# OPTIONS_GHC -Wall -Werror #-}

{- Compile instant AST to LLVM bytecode. -}

module Flatten where

import qualified Data.Map as M
import Control.Monad (foldM)

import AbsInstant ( Program (Prog)
                  , Stmt (SExp, SAss)
                  , Exp (ExpAdd, ExpMul, ExpSub, ExpDiv, ExpLit, ExpVar)
                  , CIdent (CIdent) )
import CompilerErr (CompilerErrorM, raiseCEUndefinedVariable)

-- Registers are represented as subsequent numbers.
newtype Register = RRegister Integer deriving Show
-- Counter of used registers.
newtype RegisterState = RSState Integer deriving Show
-- Map of identifiers to registers.
newtype VariableMap = VMMap (M.Map String Value)
-- Program is a list of statements.
newtype LLVMProgram = LLVMProgram [LLVMStmt] deriving Show
-- Value is either a constant or a register.
data Value = VConst Integer | VRegister Register deriving Show
data Operator = OAdd | OSub | OMul | ODiv deriving Show
-- Statement can either print a value or perform arithmetic operation
-- and write to register.
data LLVMStmt = LSArithm Value Value Operator Register
              | Print Value deriving Show


initRegister :: RegisterState
initRegister = RSState 0

newRegister :: RegisterState -> (Register, RegisterState)
newRegister (RSState num) = (RRegister num, RSState $ num + 1)

setVariable :: String -> Value -> VariableMap -> VariableMap
setVariable string value (VMMap vm) = VMMap $ M.insert string value vm

lookupVariable :: (Int, Int) -> String -> VariableMap -> CompilerErrorM Value
lookupVariable (row, col) string (VMMap vm)
 = maybe (raiseCEUndefinedVariable string row col)
        return
        (M.lookup string vm)

treeToLLVMProg :: Program -> CompilerErrorM LLVMProgram
treeToLLVMProg (Prog stmts)
  = do
        let go (stmts0, rs0, rm0) item = do
                (newStmts, rs1, rm1) <- stmtToLLVMStmt item rs0 rm0
                return (stmts0 ++ newStmts, rs1, rm1)
        (finalStmts, _, _ ) <- foldM go ([], initRegister, VMMap M.empty) stmts
        return $ LLVMProgram finalStmts

stmtToLLVMStmt :: Stmt -> RegisterState -> VariableMap
                  -> CompilerErrorM ([LLVMStmt], RegisterState, VariableMap)
stmtToLLVMStmt (SExp expr) rs0 vm = do
  (value, stmts, rs1) <- expToLLVM expr rs0 vm
  return (stmts ++ [Print value], rs1, vm)

stmtToLLVMStmt (SAss (CIdent ((_, _), ident)) expr) rs0 vm = do
  (value, stmts, rs1) <- expToLLVM expr rs0 vm
  return (stmts, rs1, setVariable ident value vm)

expToLLVM :: Exp -> RegisterState -> VariableMap
             -> CompilerErrorM (Value, [LLVMStmt], RegisterState)
expToLLVM (ExpAdd e1 e2) rs0 vm = arithmHelper OAdd e1 e2 rs0 vm
expToLLVM (ExpMul e1 e2) rs0 vm = arithmHelper OMul e1 e2 rs0 vm
expToLLVM (ExpSub e1 e2) rs0 vm = arithmHelper OSub e1 e2 rs0 vm
expToLLVM (ExpDiv e1 e2) rs0 vm = arithmHelper ODiv e1 e2 rs0 vm
expToLLVM (ExpLit num) registerState _ = return (VConst num, [], registerState)
expToLLVM (ExpVar (CIdent (pos, ident))) registerState vm
  = do var <- lookupVariable pos ident vm
       return (var, [], registerState)

arithmHelper :: Operator
                 -> Exp
                 -> Exp
                 -> RegisterState
                 -> VariableMap
                 -> CompilerErrorM (Value, [LLVMStmt], RegisterState)
arithmHelper operator e1 e2 rs0 vm = do
  (v1, s1, rs1) <- expToLLVM e1 rs0 vm
  (v2, s2, rs2) <- expToLLVM e2 rs1 vm
  let (register, newRegisterState) = newRegister rs2
  return ( VRegister register
         , s1 ++ s2 ++ [LSArithm v1 v2 operator register]
         , newRegisterState )

stringify :: LLVMProgram -> String
stringify (LLVMProgram stmts) = unlines (progHead ++ progMain ++ progTail)
  where
    progHead =
      [ "target triple = \"x86_64-apple-macosx10.12.0\""
      , ""
      , "@.str = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1"
      , ""
      , "define i32 @main() #0 {"
      ]
    progMain = map stringifyStmt stmts
    progTail =
      [ "  ret i32 0"
      , "}"
      , ""
      , "declare i32 @printf(i8*, ...) #1"
      ]
    stringifyStmt (LSArithm v1 v2 op register)
      = "  " ++ stringifyRegister register ++
        " = " ++ stringifyOp op ++
        " i32 " ++ stringifyValue v1 ++ ", " ++ stringifyValue v2
    stringifyStmt (Print val)
      = "  call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str, i32 0, i32 0), i32 " ++
        stringifyValue val ++ ")"

    stringifyRegister (RRegister num)= "%r" ++ show num
    stringifyValue (VConst num) = show num
    stringifyValue (VRegister register) = stringifyRegister register

    stringifyOp OAdd = "add"
    stringifyOp OMul = "mul"
    stringifyOp OSub = "sub"
    stringifyOp ODiv = "sdiv"

compileLLVM :: Program -> CompilerErrorM String
compileLLVM tree = do
  prog <- treeToLLVMProg tree
  return $ stringify prog
