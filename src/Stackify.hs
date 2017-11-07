module Stackify where

import AbsInstant (Program (Prog), Stmt (SExp, SAss), Exp (ExpAdd, ExpMul, ExpSub, ExpDiv, ExpLit, ExpVar), CIdent (CIdent))
import CompilerErr (CompilerErrorM, raiseCEUndefinedVariable)
import qualified Data.Map as M
import Control.Monad (foldM)

newtype Local = LNum Int deriving Show

data JVMProgram = JVMProgram { jvmProgStmts :: [JVMStmt]
                             , jvmProgStackLimit :: Int
                             , jvmProgLocalsLimit :: Int } deriving Show

data Operator = OAdd | OSub | OMul | ODiv deriving Show
data JVMStmt = Print | Const Integer | Load Local | Store Local | Arithm Operator | GetStaticPrint deriving Show

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

-- locals start with 1, we reserve 0 for main parameter being String[] args 
initLocals :: Locals
initLocals = Locals {localsNextLocal = LNum 1, localsIdentMap = M.empty}

usedLocalsNum :: Locals -> Int
usedLocalsNum Locals {localsNextLocal = LNum num, localsIdentMap = _ } = num

treeToJVMProg :: Program -> CompilerErrorM JVMProgram
treeToJVMProg (Prog stmts) = do
  let go (stmts0, locals0) item =
        do
          (newStmts, locals1) <- stmtToJVMStmt item locals0
          return (stmts0 ++ newStmts, locals1)
  (jvmStmts, finalLocals) <- foldM go ([], initLocals) stmts
  return JVMProgram { jvmProgStmts = jvmStmts
                    , jvmProgStackLimit = 100
                    , jvmProgLocalsLimit = usedLocalsNum finalLocals }

stmtToJVMStmt :: Stmt -> Locals -> CompilerErrorM ([JVMStmt], Locals)
stmtToJVMStmt (SExp expr) locals =
  do
    stmts <- expToJVM expr locals
    return ([GetStaticPrint] ++ stmts ++ [Print], locals)

stmtToJVMStmt (SAss (CIdent (_, ident)) expr) locals0 =
  do
    stmts <- expToJVM expr locals0
    let (local, locals1) = lookupLocalForWrite ident locals0
    return (stmts ++ [Store local], locals1)

-- TODO: remove VariableMap as return value
expToJVM :: Exp -> Locals -> CompilerErrorM [JVMStmt]
expToJVM (ExpAdd e1 e2) locals = arithmHelper OAdd e1 e2 locals
expToJVM (ExpMul e1 e2) locals = arithmHelper OMul e1 e2 locals
expToJVM (ExpSub e1 e2) locals = arithmHelper OSub e1 e2 locals
expToJVM (ExpDiv e1 e2) locals = arithmHelper ODiv e1 e2 locals
expToJVM (ExpLit num) _ = return [Const num]
expToJVM (ExpVar (CIdent ((row, col), ident))) locals =
  do
    local <- lookupLocalForRead row col ident locals
    return [Load local]

arithmHelper :: Operator -> Exp -> Exp -> Locals -> CompilerErrorM [JVMStmt]
arithmHelper operator e1 e2 locals =
  do
    s1 <- expToJVM e1 locals
    s2 <- expToJVM e2 locals
    return $ s1 ++ s2 ++ [Arithm operator]

stringify :: JVMProgram -> String
stringify JVMProgram { jvmProgStmts = stmts
                     , jvmProgStackLimit = stackLimit
                     , jvmProgLocalsLimit = localsLimit }
  = unlines (progHead ++ progMain ++ progTail)
  where
    progHead =
      [ ".class  public Instant"
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
    stringifyStmt GetStaticPrint = "  getstatic  java/lang/System/out Ljava/io/PrintStream;"

    stringifyOp OAdd = "iadd"
    stringifyOp OMul = "imul"
    stringifyOp OSub = "isub"
    stringifyOp ODiv = "idiv"

compileJVM tree = do
  prog <- treeToJVMProg tree
  return $ stringify prog
