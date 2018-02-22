module Tal0 where

import Control.Monad
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
  , current = Seq is o
  } = case is of
  []     -> jumpTo o
  i : is -> let rest = Seq is o in case i of
    Mov r o     -> updateReg r rest <$> fetch o f
    Add rd rs o -> fmap (updateReg rd rest) . join $ addOperand <$> Map.lookup rs f <*> fetch o f
    IfJump r o  -> do
      n <- Map.lookup r f >>= fromInt
      if n == 0
        then jumpTo o
        else return $ update rest
  where
    update s = m { current = s }
    updateReg r s o = m { file = Map.insert r o f, current = s }
    jumpTo o = fmap update $ fetch o f >>= fromLabel >>= flip Map.lookup h

addOperand :: Operand -> Operand -> Maybe Operand
addOperand o1 o2 = fmap Int $ (+) <$> fromInt o1 <*> fromInt o2
