module Tal0 where

import qualified Data.Map.Lazy as Map

newtype Register = Register Int
  deriving (Eq, Ord)

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

type HeapValue = Seq

type Heap = Map.Map Label HeapValue

data Machine = Machine
  { heap :: Heap
  , file :: File
  , seq :: Seq
  }

fetch :: Operand -> File -> Maybe Operand
fetch (Reg r) f = Map.lookup r f
fetch o _ = return o
