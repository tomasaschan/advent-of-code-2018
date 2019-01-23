module Data.Computer where

  -- newtype OpCode = OpCode Int deriving (Show,Eq)
  -- newtype Register = Register Int deriving (Show,Eq)
  -- newtype Value = Value Int deriving (Show,Eq)

  -- data Ref = R Register | V Value deriving (Show, Eq)

  -- newtype Input = Input Ref deriving (Show,Eq)
  -- newtype Output = Output Ref deriving (Show,Eq)

  -- newtype Registers = Registers [Register] deriving (Show,Eq)

  import Data.IntMap

  data Reftype = Register | Value
  data Op =
      AddR | AddI
    | MulR | MulI
    | BanR | BanI
    | BorR | BorI
    | SetR | SetI
    | GtIR | GtRI | GtRR
    | EqIR | EqRI | EqRR
    deriving (Show, Eq, Enum)

  data Instruction = Instruction Op Int Int Int deriving (Show, Eq)

  data Registry = Registry (IntMap Int) deriving (Show, Eq)

  initial :: Int -> Registry
  initial size = Registry . Data.IntMap.fromList . fmap (flip (,) 0) $ [0..size-1]

  fromList :: [Int] -> Registry
  fromList = Registry . Data.IntMap.fromList . zip [0..]
