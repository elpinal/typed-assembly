module Tal0 where

import qualified Data.Map.Lazy as Map

newtype Register = Register Int

type Label = Int

data Operand
  = Int Int
  | Label Label
  | Reg Register

data Inst
  = Mov Register Operand
  | Add { dest :: Register, src :: Register, offset :: Operand }
  | IfJump Register Operand

data Seq = Seq [Inst] Operand

type File = Map.Map Register Operand

type Heap = Map.Map Label Seq
