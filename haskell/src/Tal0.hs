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

fromInt :: Operand -> Maybe Int
fromInt (Int n) = return n
fromInt _ = Nothing

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
eval1 m @ Machine
  { heap = h
  , file = f
  , current = Seq (Mov r o : is) o0
  } = fmap update $ fetch o f
  where
    update o = m { file = Map.insert r o f, current = Seq is o0 }
eval1 m @ Machine
  { heap = h
  , file = f
  , current = Seq (Add rd rs o : is) o0
  } = do
    n1 <- Map.lookup rs f >>= fromInt
    n2 <- fetch o f >>= fromInt
    return . update . Int $ n1 + n2
  where
    update o = m { file = Map.insert rd o f, current = Seq is o0 }
eval1 m @ Machine
  { heap = h
  , file = f
  , current = Seq (IfJump r o : is) o0
  } = do
    n <- Map.lookup r f >>= fromInt
    if n == 0
      then fmap update $ fetch o f >>= fromLabel >>= flip Map.lookup h
      else return . update $ Seq is o0
  where
    update s = m { current = s }
