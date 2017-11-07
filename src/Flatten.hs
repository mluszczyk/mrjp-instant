module Flatten where

import AbsInstant (Program (Prog), Stmt (SExp, SAss), Exp (ExpAdd, ExpMul, ExpSub, ExpDiv, ExpLit, ExpVar), CIdent (CIdent))
import qualified Data.Map as M
import CompilerErr (CompilerErrorM, raiseCEUndefinedVariable)
import Control.Monad (foldM)

initRegister = RSState 0
newRegister (RSState num) = (RRegister num, RSState $ num + 1)

newtype Register = RRegister Integer deriving Show
newtype RegisterState = RSState Integer deriving Show
newtype VariableMap = VMMap (M.Map String Value)

newtype LLVMProgram = LLVMProgram [LLVMStmt] deriving Show

setVariable :: String -> Value -> VariableMap -> VariableMap
setVariable string value (VMMap vm) = VMMap $ M.insert string value vm

lookupVariable :: (Int, Int) -> String -> VariableMap -> CompilerErrorM Value
lookupVariable (row, col) string (VMMap vm)
 = maybe (raiseCEUndefinedVariable string row col :: (CompilerErrorM a))
        (return :: (Value -> CompilerErrorM Value))
        (M.lookup string vm :: (Maybe Value))
-- TODO: remove types above

-- TODO: remove VConst?
data Value = VConst Integer | VRegister Register deriving Show

data Operator = OAdd | OSub | OMul | ODiv deriving Show

data LLVMStmt = LSArithm Value Value Operator Register | Print Value deriving Show

treeToLLVMProg :: Program -> CompilerErrorM LLVMProgram
treeToLLVMProg (Prog stmts)
  = do
        let go (stmts0, rs0, rm0) item = do
                (newStmts, rs1, rm1) <- stmtToLLVMStmt item rs0 rm0
                return (stmts0 ++ newStmts, rs1, rm1)
        (finalStmts, _, _ ) <- foldM go ([], initRegister, VMMap M.empty) stmts
        return $ LLVMProgram finalStmts

stmtToLLVMStmt :: Stmt -> RegisterState -> VariableMap -> CompilerErrorM ([LLVMStmt], RegisterState, VariableMap)
stmtToLLVMStmt (SExp expr) rs0 vm0 = do
  (value, stmts, rs1, vm1) <- expToLLVM expr rs0 vm0
  return (stmts ++ [Print value], rs1, vm1)

-- TODO: write to register?
stmtToLLVMStmt (SAss (CIdent ((_, _), ident)) expr) rs0 vm0 = do
  (value, stmts, rs1, vm1) <- expToLLVM expr rs0 vm0
  return (stmts, rs1, setVariable ident value vm1)

-- TODO: remove VariableMap as return value
expToLLVM :: Exp -> RegisterState -> VariableMap -> CompilerErrorM (Value, [LLVMStmt], RegisterState, VariableMap)
expToLLVM (ExpAdd e1 e2) rs0 vm0 = arithmHelper OAdd e1 e2 rs0 vm0
expToLLVM (ExpMul e1 e2) rs0 vm0 = arithmHelper OMul e1 e2 rs0 vm0
expToLLVM (ExpSub e1 e2) rs0 vm0 = arithmHelper OSub e1 e2 rs0 vm0
expToLLVM (ExpDiv e1 e2) rs0 vm0 = arithmHelper ODiv e1 e2 rs0 vm0
expToLLVM (ExpLit num) registerState vm = return (VConst num, [], registerState, vm)
expToLLVM (ExpVar (CIdent (pos, ident))) registerState vm
  = do var <- lookupVariable pos ident vm
       return (var, [], registerState, vm)

arithmHelper operator e1 e2 rs0 vm0 = do
  (v1, s1, rs1, vm1) <- expToLLVM e1 rs0 vm0
  (v2, s2, rs2, vm2) <- expToLLVM e2 rs1 vm1
  let (register, newRegisterState) = newRegister rs2
  return ( VRegister register
         , s1 ++ s2 ++ [LSArithm v1 v2 operator register]
         , newRegisterState
         , vm2 )

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
    stringifyStmt (LSArithm v1 v2 op register) = "  " ++ stringifyRegister register ++ " = " ++ stringifyOp op ++ " i32 " ++ stringifyValue v1 ++ ", " ++ stringifyValue v2
    stringifyStmt (Print val) = "  call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str, i32 0, i32 0), i32 " ++ stringifyValue val ++ ")"

    stringifyRegister (RRegister num)= "%r" ++ show num
    stringifyValue (VConst num) = show num
    stringifyValue (VRegister register) = stringifyRegister register  -- TODO: give prefixes to register names

    stringifyOp OAdd = "add"
    stringifyOp OMul = "mul"
    stringifyOp OSub = "sub"
    stringifyOp ODiv = "sdiv"

compileLLVM tree = do
  prog <- treeToLLVMProg tree
  return $ stringify prog
