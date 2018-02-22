module Tal0 where

import Control.Monad
import qualified Data.Map.Lazy as Map

newtype Register = Register Int
  deriving (Eq, Ord, Show)

type Label = Int

data Operand
  = Int Int
  | Label Label
  | Reg Register
  deriving (Eq, Show)

fromLabel :: Operand -> Either EvalError Label
fromLabel (Label l) = return l
fromLabel o = Left $ NotLabel o

fromInt :: Operand -> Either EvalError Int
fromInt (Int n) = return n
fromInt o = Left $ NotInt o

data Inst
  = Mov Register Operand
  | Add { dest :: Register, src :: Register, offset :: Operand }
  | IfJump Register Operand
  deriving (Eq, Show)

data Seq = Seq [Inst] Operand
  deriving (Eq, Show)

type File = Map.Map Register Operand

type HeapValue = Seq

type Heap = Map.Map Label HeapValue

data Machine = Machine
  { heap :: Heap
  , file :: File
  , current :: Seq
  }
  deriving (Eq, Show)

data EvalError
  = NotInt Operand
  | NotLabel Operand
  | NoRegister Register
  | NoLabel Label
  deriving (Eq, Show)

rightOr :: Maybe a -> b -> Either b a
rightOr Nothing y = Left y
rightOr (Just x) _ = Right x

register :: File -> Register -> Either EvalError Operand
register f r = Map.lookup r f `rightOr` NoRegister r

heapValue :: Heap -> Label -> Either EvalError HeapValue
heapValue h l = Map.lookup l h `rightOr` NoLabel l

fetch :: Operand -> File -> Either EvalError Operand
fetch (Reg r) f = register f r
fetch o _ = return o

eval1 :: Machine -> Either EvalError Machine
eval1 m @ Machine
  { heap = h
  , file = f
  , current = s
  } = case first s of
  Left o          -> jumpTo o
  Right (i, rest) -> case i of
    Mov r o     -> updateReg r rest <$> fetch o f
    Add rd rs o -> fmap (updateReg rd rest) . join $ addOperand <$> register f rs <*> fetch o f
    IfJump r o  -> do
      n <- register f r >>= fromInt
      if n == 0
        then jumpTo o
        else return $ update rest
  where
    update s = m { current = s }
    updateReg r s o = m { file = Map.insert r o f, current = s }
    jumpTo o = fmap update $ fetch o f >>= fromLabel >>= heapValue h

first :: Seq -> Either Operand (Inst, Seq)
first (Seq is o) = case is of
  []     -> Left o
  i : is -> Right (i, Seq is o)

addOperand :: Operand -> Operand -> Either EvalError Operand
addOperand o1 o2 = fmap Int $ (+) <$> fromInt o1 <*> fromInt o2
