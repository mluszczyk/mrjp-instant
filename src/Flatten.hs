module Flatten where

import AbsInstant (Program (Prog), Stmt (SExp, SAss), Exp (ExpAdd, ExpMul, ExpSub, ExpDiv, ExpLit, ExpVar), Ident (Ident))
import qualified Data.Map as M

initRegister = RSState 0
newRegister (RSState num) = (RRegister num, RSState $ num + 1)

newtype Register = RRegister Integer deriving Show
newtype RegisterState = RSState Integer deriving Show
newtype VariableMap = VMMap (M.Map String Value)

newtype LLVMProgram = LLVMProgram [LLVMStmt] deriving Show

setVariable :: String -> Value -> VariableMap -> VariableMap
setVariable string value (VMMap vm) = VMMap $ M.insert string value vm

lookupVariable :: String -> VariableMap -> Value
lookupVariable string (VMMap vm) = vm M.! string

-- TODO: remove VConst?
data Value = VConst Integer | VRegister Register deriving Show

data Operator = OAdd | OSub | OMul | ODiv deriving Show

data LLVMStmt = LSArithm Value Value Operator Register | Print Value deriving Show

treeToLLVMProg :: Program -> LLVMProgram
treeToLLVMProg (Prog stmts) = LLVMProgram $ fstOfThree $ foldl go ([], initRegister, VMMap M.empty) stmts
  where go (stmts0, rs0, rm0) item = let (newStmts, rs1, rm1) = stmtToLLVMStmt item rs0 rm0 in (stmts0 ++ newStmts, rs1, rm1)
        fstOfThree (a, _, _) = a

stmtToLLVMStmt :: Stmt -> RegisterState -> VariableMap -> ([LLVMStmt], RegisterState, VariableMap)
stmtToLLVMStmt (SExp expr) rs0 vm0 =
        (stmts ++ [Print value], rs1, vm1)
  where (value, stmts, rs1, vm1) = expToLLVM expr rs0 vm0
-- TODO: write to register?
stmtToLLVMStmt (SAss (Ident ident) expr) rs0 vm0 = (stmts, rs1, setVariable ident value vm1)
  where (value, stmts, rs1, vm1) = expToLLVM expr rs0 vm0

-- TODO: remove VariableMap as return value
expToLLVM :: Exp -> RegisterState -> VariableMap -> (Value, [LLVMStmt], RegisterState, VariableMap)
expToLLVM (ExpAdd e1 e2) rs0 vm0 = arithmHelper OAdd e1 e2 rs0 vm0
expToLLVM (ExpMul e1 e2) rs0 vm0 = arithmHelper OMul e1 e2 rs0 vm0
expToLLVM (ExpSub e1 e2) rs0 vm0 = arithmHelper OSub e1 e2 rs0 vm0
expToLLVM (ExpDiv e1 e2) rs0 vm0 = arithmHelper ODiv e1 e2 rs0 vm0
expToLLVM (ExpLit num) registerState vm = (VConst num, [], registerState, vm)
expToLLVM (ExpVar (Ident ident)) registerState vm = (lookupVariable ident vm, [], registerState, vm)

arithmHelper operator e1 e2 rs0 vm0 = (
      VRegister register, s1 ++ s2 ++ [LSArithm v1 v2 operator register], newRegisterState, vm2)
  where (v1, s1, rs1, vm1) = expToLLVM e1 rs0 vm0
        (v2, s2, rs2, vm2) = expToLLVM e2 rs1 vm1
        (register, newRegisterState) = newRegister rs2

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

compileLLVM = stringify . treeToLLVMProg
