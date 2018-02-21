module Tal0 where

newtype Register = Register Int

data Operand
  = Int Int
  | Label Int
  | Reg Register

data Inst
  = Mov Register Operand
  | Add { dest :: Register, src :: Register, offset :: Operand }
  | IfJump Register Operand

data Seq = Seq [Inst] Operand
