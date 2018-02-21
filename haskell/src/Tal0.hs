module Tal0 where

newtype Register = Register Int

data Operand
  = Int Int
  | Label Int
  | Reg Register
