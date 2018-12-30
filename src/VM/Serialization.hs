{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
module VM.Serialization
( asmMnemonic
, machineCode
) where
import VM.InstructionSet
import GHC.Generics
import Data.Char(toLower)

-- | Converts a number into binary, with a fixed length
bitsFixedWidth :: Int -> MInt -> String
bitsFixedWidth 0 _ = []
bitsFixedWidth n p = bitsFixedWidth (n - 1) q ++ show r
    where (q, r) = p `divMod` 2

-- | Provides a textual mnemonic for the given instruction
asmMnemonic :: Op -> String
asmMnemonic = asmOp

-- | The instruction encoded into machine code, with 'X'
-- | representing unused bits
machineCode :: Op -> String
machineCode x = code ++ padding
    where
        code = machineOp (from x)
        padding = replicate (32 - length code) 'X'

-- | Helper typeclass to convert an arbitrary instruction into it's
-- | equivalent machine code
class MachineCode a where
    machineOp :: a -> String

instance MachineCode (f p) => MachineCode (C1 ('MetaCons "LDI" a b) f p) where
    machineOp x = bitsFixedWidth 5 0 ++ machineOp (unM1 x)

instance MachineCode (f p) => MachineCode (C1 ('MetaCons "ADD" a b) f p) where
    machineOp x = bitsFixedWidth 5 1 ++ machineOp (unM1 x)

instance MachineCode (f p) => MachineCode (C1 ('MetaCons "MUL" a b) f p) where
    machineOp x = bitsFixedWidth 5 2 ++ machineOp (unM1 x)

instance MachineCode (f p) => MachineCode (C1 ('MetaCons "CMP" a b) f p) where
    machineOp x = bitsFixedWidth 5 3 ++ machineOp (unM1 x)

instance MachineCode (f p) => MachineCode (C1 ('MetaCons "JGT" a b) f p) where
    machineOp x = bitsFixedWidth 5 4 ++ machineOp (unM1 x)

instance MachineCode (f p) => MachineCode (C1 ('MetaCons "SUB" a b) f p) where
    machineOp x = bitsFixedWidth 5 5 ++ machineOp (unM1 x)

instance {-# OVERLAPPABLE #-} MachineCode (f p)=> MachineCode (C1 c f p) where
    machineOp x = "?????" ++ machineOp (unM1 x)

instance MachineCode (f p) => MachineCode (D1 c f p) where
    machineOp = machineOp . unM1

instance MachineCode (f p) => MachineCode (S1 c f p) where
    machineOp = machineOp . unM1

instance MachineCode c => MachineCode (K1 i c p) where
    machineOp = machineOp . unK1

instance (MachineCode (f p), MachineCode (g p)) => MachineCode ((:+:) f g p) where
    machineOp (L1 x) = machineOp x
    machineOp (R1 x) = machineOp x

instance (MachineCode (f p), MachineCode (g p)) => MachineCode ((:*:) f g p) where
    machineOp (a :*: b) = machineOp a ++ machineOp b

instance MachineCode Reg where
    machineOp (Reg reg) = bitsFixedWidth 5 reg

instance MachineCode Imm where
    machineOp (Imm imm) = bitsFixedWidth 22 imm


-- | Helper typeclass to define a textual representation
-- | of an arbitrary machine op
class Assembly a where
    asmOp :: a -> String

instance Assembly Imm where
    asmOp (Imm imm) = show imm

instance Assembly Reg where
    asmOp (Reg reg) = "%r" ++ show reg

instance Assembly Op where
    asmOp = asmOp . from

instance (Constructor c, Assembly (f p)) => Assembly (C1 c f p) where
    asmOp x = unwords [map toLower (conName x), asmOp (unM1 x)]

instance Assembly (f p) => Assembly (D1 c f p) where
    asmOp = asmOp . unM1

instance Assembly (f p) => Assembly (S1 c f p) where
    asmOp = asmOp . unM1

instance Assembly c => Assembly (K1 i c p) where
    asmOp = asmOp . unK1


instance (Assembly (f p), Assembly (g p)) => Assembly ((:+:) f g p) where
    asmOp (L1 x) = asmOp x
    asmOp (R1 x) = asmOp x

instance (Assembly (f p), Assembly (g p)) => Assembly ((:*:) f g p) where
    asmOp (a :*: b) = concat [asmOp a, ", ", asmOp b]
