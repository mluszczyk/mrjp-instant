module CompilerErr where

data CompilerError = CEUndefinedVariable { ceIdent :: String
                                         , ceLine :: Integer
                                         , ceColumn :: Integer }

type CompilerErrorM a = Either CompilerError a

raiseCEUndefinedVariable :: String -> Integer -> Integer -> CompilerErrorM a
raiseCEUndefinedVariable ident line column = Left CEUndefinedVariable { ceIdent = ident
                                                                      , ceLine = line
                                                                      , ceColumn = column }

errorToString :: CompilerError -> String
errorToString CEUndefinedVariable { ceIdent = ident
                                  , ceLine = line
                                  , ceColumn = column }
 = "undefined variable " ++ ident ++ " on line " ++ show line ++ " column " ++ show column
