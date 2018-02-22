module Tal0.Parser
  () where

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Token

data Token
  = Ident
  | Jump

def :: LanguageDef st
def = emptyDef
  { reservedNames = ["jump"]
  }

lexer :: TokenParser st
lexer = makeTokenParser def
