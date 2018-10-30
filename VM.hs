{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module VM where
import InstructionSet
import Serialization
import AssemblyParser

import GHC.Generics
import GHC.Types(Symbol)
import Data.Array.Base
import Data.Array.ST
import Data.Array.Unboxed
import Data.Bits


data VM = VM {
      regs :: UArray MInt MInt
    , text :: Array MInt Op
} deriving (Eq, Show)

sp = Reg 13
lr = Reg 14
pc = Reg 15

-- | Registers:
-- | 1-12 general purpose
-- | 13 stack pointer
-- | 14 link register
-- | 15 program counter

-- | Adds the provided number to the program counter
increaseProgramCounter :: MInt -> VM -> VM
increaseProgramCounter n vm = vm { regs = regs' }
    where
        regs' = runSTUArray $ do
            registers <- unsafeThaw (regs vm)
            pc <- unsafeRead registers 15
            unsafeWrite registers 15 (pc + n)
            return registers

regStore :: VM -> Reg -> MInt -> VM
regStore vm (Reg reg) val = vm { regs = regs' }
    where
        regs' = runSTUArray $ do
            registers <- unsafeThaw (regs vm)
            unsafeWrite registers (fromIntegral $ reg) val
            return registers


programOffset vm = content vm pc `div` 4

-- | The content of an immediate, or the value at a register
class Content a where
    content :: VM -> a -> MInt

instance Content Imm where
    content _ (Imm imm) = imm

instance Content Reg where
    content vm (Reg reg) = regs vm ! reg

data VMExit = SegmentationFault deriving (Eq, Show)

step :: VM -> Either VMExit VM
step vm
    | not $ inRange (bounds $ text vm) (programOffset vm) = Left SegmentationFault
    | otherwise = Right $ execute (text vm ! programOffset vm) vm

cmp a b = if a < b then 1 else 0
      .|. if a > b then 2 else 0
      .|. if a == b then 4 else 0

regBinaryOp f d a b vm = regStore vm d (f (content vm a) (content vm b))

execute :: Op -> VM -> VM
execute (ADD d a b) vm = regBinaryOp (+) d a b vm
execute (SUB d a b) vm = regBinaryOp (-) d a b vm
execute (MUL d a b) vm = regBinaryOp (*) d a b vm
execute (CMP d a b) vm = regBinaryOp cmp d a b vm
execute (LDI d imm) vm = regStore vm d (content vm imm)
execute (JGT d imm) vm = case (content vm d .|. 1) == 1 of
    True -> regBinaryOp (+) pc pc imm vm
    False -> vm
