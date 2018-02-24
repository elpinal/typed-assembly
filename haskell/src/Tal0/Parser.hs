module Tal0.Parser
  () where

import Tal0

import Text.Parsec hiding (label)
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token

import Data.Char

data Token
  = Ident
  | Jump

def :: LanguageDef st
def = emptyDef
  { reservedNames = ["if", "jump"]
  , reservedOpNames = [":=", "+", ";"]
  }

lexer :: TokenParser st
lexer = makeTokenParser def

jump :: Parser ()
jump = reserved lexer "jump"

ident :: Parser String
ident = identifier lexer

lastInst :: Parser Operand
lastInst = jump >> operand

inst :: Parser Inst
inst = choice
  [ mov
  , add
  , ifJump
  ]

mov :: Parser Inst
mov = lookAhead $ do
  r <- register
  assign
  o <- operand
  return $ Mov r o

add :: Parser Inst
add = lookAhead $ do
  rd <- register
  assign
  rs <- register
  plus
  o <- operand
  return $ Add rd rs o

ifJump :: Parser Inst
ifJump = do
  reserved lexer "if"
  r <- register
  jump
  o <- operand
  return $ IfJump r o

register :: Parser Register
register = do
  s <- ident
  if isRegister s
    then return . Register $ read $ drop 1 s -- TODO: subtleness of `read`
    else unexpected "label"

isRegister :: String -> Bool
isRegister ('r' : xs) | all isDigit xs && length xs > 0 = True
isRegister _ = False

operand :: Parser Operand
operand = choice
  [ Reg <$> try register
  , Label <$> label
  , Int <$> int
  ]

int :: Parser Int
int = fromIntegral <$> integer lexer

label :: Parser String
label = do
  s <- ident
  if isRegister s
    then unexpected "register"
    else return s

assign :: Parser ()
assign = reservedOp lexer ":="

plus :: Parser ()
plus = reservedOp lexer "+"