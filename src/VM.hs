{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module VM where
import VM.InstructionSet
import VM.Serialization
import VM.Parser

import GHC.Generics
import GHC.Types(Symbol)
import Data.Array.Base
import Data.Array.ST
import Data.Array.Unboxed
import Data.Bits


data VM = VM {
      regs :: UArray MInt MInt
    , mem :: UArray MInt MInt
    , text :: Array MInt Op
} deriving (Eq, Show)

sp = Reg 13
lr = Reg 14
pc = Reg 15

-- | Instruction width in bytes
instructionWidth = 4

-- | Registers:
-- | 1-12 general purpose
-- | 13 stack pointer
-- | 14 link register
-- | 15 program counter

-- | Adds the provided number to the program counter
increaseProgramCounter :: MInt -> VM -> VM
increaseProgramCounter n vm = vm { regs = regs' }
    where
        regs' = regs vm // [(15, (regs vm ! 15) + n)]

regStore :: VM -> Reg -> MInt -> VM
regStore vm (Reg reg) val = vm { regs = regs' }
    where
        regs' = regs vm // [(fromIntegral reg, val)]

memStore :: VM -> MInt -> MInt -> VM
memStore vm val offset = vm { mem = mem' }
    where
        mem' = mem vm // [(fromIntegral offset, val)]


programOffset vm = offset vm pc
offset vm reg = content vm reg `div` instructionWidth

-- | The content of an immediate, or the value at a register
class Content a where
    content :: VM -> a -> MInt

instance Content Imm where
    content _ (Imm imm) = imm

instance Content Reg where
    content vm (Reg reg) = regs vm ! reg

data VMExit = SegmentationFault
            | UserExit
    deriving (Eq, Show)

step :: VM -> Either VMExit VM
step vm
    | not $ inRange (bounds $ text vm) (programOffset vm) = Left SegmentationFault
    | otherwise = execute (text vm ! programOffset vm) vm'
    where vm' = increaseProgramCounter instructionWidth vm

cmp a b = if a < b then 1 else 0
      .|. if a > b then 2 else 0
      .|. if a == b then 4 else 0

regBinaryOp f d a b vm = Right $ regStore vm d (f (content vm a) (content vm b))

execute :: Op -> VM -> Either VMExit VM
execute (ADD d a b) vm = regBinaryOp (+) d a b vm
execute (SUB d a b) vm = regBinaryOp (-) d a b vm
execute (MUL d a b) vm = regBinaryOp (*) d a b vm
execute (CMP d a b) vm = regBinaryOp cmp d a b vm
execute (LDI d imm) vm = Right $ regStore vm d (content vm imm)
execute (JGT d imm) vm = case (content vm d .|. 2) == 2 of
    True -> regBinaryOp (+) pc pc imm vm
    False -> Right vm
execute (LOAD d a) vm
    | not $ inRange (bounds $ mem vm) (offset vm a) = Left SegmentationFault
    | otherwise = Right $ regStore vm d (mem vm ! offset vm a)
execute (STORE s d) vm
    | not $ inRange (bounds $ mem vm) (offset vm d) = Left SegmentationFault
    | otherwise = Right $ memStore vm (content vm s) (offset vm d)
