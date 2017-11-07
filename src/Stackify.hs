module Stackify where

import AbsInstant (Program (Prog), Stmt (SExp, SAss), Exp (ExpAdd, ExpMul, ExpSub, ExpDiv, ExpLit, ExpVar), Ident (Ident))
import qualified Data.Map as M

newtype Local = LNum Integer deriving Show

data JVMProgram = JVMProgram { jvmProgStmts :: [JVMStmt]
                             , jvmProgStackLimit :: Integer
                             , jvmProgLocalsLimit :: Integer } deriving Show

data Operator = OAdd | OSub | OMul | ODiv deriving Show
data JVMStmt = Print | Const Integer | Load Local | Store Local | Arithm Operator deriving Show

data Locals = Locals { localsNextLocal :: Local
                     , localsIdentMap :: M.Map String Local }

lookupLocalForWrite :: String -> Locals -> (Local, Locals)
lookupLocalForWrite ident locals@Locals {localsNextLocal = LNum nextLocalNum, localsIdentMap = identMap}
  = if ident `M.member` identMap then (identMap M.! ident, locals)
    else (LNum nextLocalNum, Locals { localsNextLocal = LNum (nextLocalNum + 1)
                                    , localsIdentMap = M.insert ident (LNum nextLocalNum) identMap })

lookupLocalForRead :: String -> Locals -> Local
lookupLocalForRead ident Locals {localsNextLocal = nextLocal, localsIdentMap = identMap}
 = identMap M.! ident

initLocals :: Locals
initLocals = Locals {localsNextLocal = LNum 0, localsIdentMap = M.empty}

treeToJVMProg :: Program -> JVMProgram
treeToJVMProg (Prog stmts) = JVMProgram { jvmProgStmts = fst $ foldl go ([], initLocals) stmts
                                        , jvmProgStackLimit = 20
                                        , jvmProgLocalsLimit = 20 }
  where go (stmts0, locals0) item = let (newStmts, locals1) = stmtToLLVMStmt item locals0 in (stmts0 ++ newStmts, locals1)

stmtToLLVMStmt :: Stmt -> Locals -> ([JVMStmt], Locals)
stmtToLLVMStmt (SExp expr) locals = (stmts ++ [Print], locals)
  where stmts = expToLLVM expr locals
stmtToLLVMStmt (SAss (Ident ident) expr) locals0 = (stmts ++ [Store local], locals1)
  where stmts = expToLLVM expr locals0
        (local, locals1) = lookupLocalForWrite ident locals0

-- TODO: remove VariableMap as return value
expToLLVM :: Exp -> Locals -> [JVMStmt]
expToLLVM (ExpAdd e1 e2) locals = arithmHelper OAdd e1 e2 locals
expToLLVM (ExpMul e1 e2) locals = arithmHelper OMul e1 e2 locals
expToLLVM (ExpSub e1 e2) locals = arithmHelper OSub e1 e2 locals
expToLLVM (ExpDiv e1 e2) locals = arithmHelper ODiv e1 e2 locals
expToLLVM (ExpLit num) _ = [Const num]
expToLLVM (ExpVar (Ident ident)) locals = [Load local]
  where local = lookupLocalForRead ident locals

arithmHelper :: Operator -> Exp -> Exp -> Locals -> [JVMStmt]
arithmHelper operator e1 e2 locals =
      s1 ++ s2 ++ [Arithm operator]
  where s1 = expToLLVM e1 locals
        s2 = expToLLVM e2 locals

stringify :: JVMProgram -> String
stringify JVMProgram { jvmProgStmts = stmts
                     , jvmProgStackLimit = stackLimit
                     , jvmProgLocalsLimit = localsLimit }
  = unlines (progHead ++ progMain ++ progTail)
  where
    progHead =
      [ ".class  public Hello"
      , ".super  java/lang/Object"
      , ""
      , "; standard initializer"
      , ".method public <init>()V"
      , "  aload_0"
      , "  invokespecial java/lang/Object/<init>()V"
      , "  return"
      , ".end method"
      , ""
      , ".method public static main([Ljava/lang/String;)V"
      , ".limit stack " ++ show stackLimit
      , ".limit locals " ++ show localsLimit
      , "  getstatic  java/lang/System/out Ljava/io/PrintStream;"
      ]
    progMain = map stringifyStmt stmts
    progTail =
      [ "  return"
      , ".end method"
      ]
    stringifyStmt Print = "  invokevirtual  java/io/PrintStream/println(I)V"
    stringifyStmt (Const num) = "  ldc " ++ show num
    stringifyStmt (Load (LNum localNum)) = "  iload " ++ show localNum
    stringifyStmt (Store (LNum localNum)) = "  istore " ++ show localNum
    stringifyStmt (Arithm operator) = "  " ++ stringifyOp operator

    stringifyOp OAdd = "iadd"
    stringifyOp OMul = "imul"
    stringifyOp OSub = "isub"
    stringifyOp ODiv = "idiv"

compileJVM = stringify . treeToJVMProg
