module Stackify where

import AbsInstant (Program (Prog), Stmt (SExp, SAss), Exp (ExpAdd, ExpMul, ExpSub, ExpDiv, ExpLit, ExpVar), CIdent (CIdent))
import CompilerErr (CompilerErrorM, raiseCEUndefinedVariable)
import qualified Data.Map as M
import Control.Monad (foldM)

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

lookupLocalForRead :: Int -> Int -> String -> Locals -> CompilerErrorM Local
lookupLocalForRead row col ident Locals {localsNextLocal = nextLocal, localsIdentMap = identMap}
 = maybe (raiseCEUndefinedVariable ident row col) return (M.lookup ident identMap)

initLocals :: Locals
initLocals = Locals {localsNextLocal = LNum 0, localsIdentMap = M.empty}

treeToJVMProg :: Program -> CompilerErrorM JVMProgram
treeToJVMProg (Prog stmts) = do
  let go (stmts0, locals0) item =
        do
          (newStmts, locals1) <- stmtToLLVMStmt item locals0
          return (stmts0 ++ newStmts, locals1)
  (jvmStmts, _) <- foldM go ([], initLocals) stmts
  return JVMProgram { jvmProgStmts = jvmStmts
                    , jvmProgStackLimit = 20
                    , jvmProgLocalsLimit = 20 }

stmtToLLVMStmt :: Stmt -> Locals -> CompilerErrorM ([JVMStmt], Locals)
stmtToLLVMStmt (SExp expr) locals =
  do
    stmts <- expToLLVM expr locals
    return (stmts ++ [Print], locals)

stmtToLLVMStmt (SAss (CIdent (_, ident)) expr) locals0 =
  do
    stmts <- expToLLVM expr locals0
    let (local, locals1) = lookupLocalForWrite ident locals0
    return (stmts ++ [Store local], locals1)

-- TODO: remove VariableMap as return value
expToLLVM :: Exp -> Locals -> CompilerErrorM [JVMStmt]
expToLLVM (ExpAdd e1 e2) locals = arithmHelper OAdd e1 e2 locals
expToLLVM (ExpMul e1 e2) locals = arithmHelper OMul e1 e2 locals
expToLLVM (ExpSub e1 e2) locals = arithmHelper OSub e1 e2 locals
expToLLVM (ExpDiv e1 e2) locals = arithmHelper ODiv e1 e2 locals
expToLLVM (ExpLit num) _ = return [Const num]
expToLLVM (ExpVar (CIdent ((row, col), ident))) locals =
  do
    local <- lookupLocalForRead row col ident locals
    return [Load local]

arithmHelper :: Operator -> Exp -> Exp -> Locals -> CompilerErrorM [JVMStmt]
arithmHelper operator e1 e2 locals =
  do
    s1 <- expToLLVM e1 locals
    s2 <- expToLLVM e2 locals
    return $ s1 ++ s2 ++ [Arithm operator]

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

compileJVM tree = do
  prog <- treeToJVMProg tree
  return $ stringify prog
