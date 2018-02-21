module Tal0 where

import qualified Data.Map.Lazy as Map

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

type File = Map.Map Register Operand
