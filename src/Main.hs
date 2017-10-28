module Main where

import Text.ParserCombinators.Parsec

type TNumber = Int

type TIdent = String

data TOperator = TAdd
               | TSubtract
               | TMultiply
               | TDivide
                 deriving (Eq, Ord, Show)

data TExpression = TNode TExpression TOperator TExpression
                 | TTerminal TNumber
                 | TRef TIdent
                   deriving (Show)

data TStatement = TExp TExpression
                | TAssign TIdent TExpression
                  deriving (Show)

type TProgram = [TStatement]

numberParser:: GenParser Char st TNumber
numberParser = read <$> many1 (oneOf "0123456789")

operatorParser:: GenParser Char st TOperator
operatorParser = chooseOp <$> oneOf "+-*/"
                  where chooseOp '+' = TAdd
                        chooseOp '-' = TSubtract
                        chooseOp '*' = TMultiply
                        chooseOp '/' = TDivide

expressionParser:: GenParser Char st TExpression
expressionParser = between (char '(') (char ')') binaryExpressionParser <|>
                   (TRef <$> identParser) <|>
                   (TTerminal <$> numberParser)

binaryExpressionParser:: GenParser Char st TExpression
binaryExpressionParser = TNode <$> expressionParser <*> operatorParser <*> expressionParser

identParser:: GenParser Char st TIdent
identParser = many1 (oneOf "asdfghjklzxcvbnmqwertyuiop")

statementParser:: GenParser Char st TStatement
statementParser = (TExp <$> expressionParser) <|>
                  TAssign <$> identParser <*> (char '=' >> expressionParser)

parseCSV :: String -> Either ParseError TStatement
parseCSV  = parse (statementParser <* eof) "(unknown)"

main :: IO ()
main =
  print $ parseCSV "a5"
