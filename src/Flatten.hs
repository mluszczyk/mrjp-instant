module Flatten where

import AbsInstant (Program (Prog), Stmt (SExp, SAss), Exp (ExpAdd, ExpMul, ExpLit, ExpVar), Ident (Ident))
import qualified Data.Map as M

initRegister = RSState 0
newRegister (RSState num) = (RRegister num, RSState $ num + 1)

newtype Register = RRegister Integer deriving Show
newtype RegisterState = RSState Integer deriving Show
newtype VariableMap = VMMap (M.Map String Value)

setVariable :: String -> Value -> VariableMap -> VariableMap
setVariable string value (VMMap vm) = VMMap $ M.insert string value vm

lookupVariable :: String -> VariableMap -> Value
lookupVariable string (VMMap vm) = vm M.! string

data Value = VConst Integer | VRegister Register deriving Show

data LLVMStmt = DebugExp | DebugAss | LSAdd Value Value Register | Print Value deriving Show

treeToLLVMProg :: Program -> [LLVMStmt]
treeToLLVMProg (Prog stmts) = fstOfThree $ foldl go ([], initRegister, VMMap M.empty) stmts
  where go (stmts0, rs0, rm0) item = let (newStmts, rs1, rm1) = stmtToLLVMStmt item rs0 rm0 in (stmts0 ++ newStmts, rs1, rm1)
        fstOfThree (a, _, _) = a

stmtToLLVMStmt :: Stmt -> RegisterState -> VariableMap -> ([LLVMStmt], RegisterState, VariableMap)
stmtToLLVMStmt (SExp expr) rs0 vm0 =
        (stmts ++ [Print value], rs1, vm1)
  where (value, stmts, rs1, vm1) = expToLLVM expr rs0 vm0

stmtToLLVMStmt (SAss (Ident ident) expr) rs0 vm0 = (stmts, rs1, setVariable ident value vm1)
  where (value, stmts, rs1, vm1) = expToLLVM expr rs0 vm0

-- remove VariableMap as return value
expToLLVM :: Exp -> RegisterState -> VariableMap -> (Value, [LLVMStmt], RegisterState, VariableMap)
expToLLVM (ExpAdd e1 e2) rs0 vm0 = (
      VRegister register, s1 ++ s2 ++ [LSAdd v1 v2 register], newRegisterState, vm2)
  where (v1, s1, rs1, vm1) = expToLLVM e1 rs0 vm0
        (v2, s2, rs2, vm2) = expToLLVM e2 rs1 vm1
        (register, newRegisterState) = newRegister rs2

expToLLVM (ExpLit num) registerState vm = (VConst num, [], registerState, vm)
expToLLVM (ExpVar (Ident ident)) registerState vm = (lookupVariable ident vm, [], registerState, vm)
expToLLVM _ registerState vm = (VConst 41, [DebugExp], registerState, vm)
