module Tal0 where

import qualified Data.Map.Lazy as Map

newtype Register = Register Int
  deriving (Eq, Ord)

type Label = Int

data Operand
  = Int Int
  | Label Label
  | Reg Register

fromLabel :: Operand -> Maybe Label
fromLabel (Label l) = return l
fromLabel _ = Nothing

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
  , current :: Seq
  }

fetch :: Operand -> File -> Maybe Operand
fetch (Reg r) f = Map.lookup r f
fetch o _ = return o

eval1 :: Machine -> Maybe Machine
eval1 m @ Machine
  { heap = h
  , file = f
  , current = Seq [] o
  } = fmap update $ fetch o f >>= fromLabel >>= flip Map.lookup h
    where
      update s = m { current = s }
