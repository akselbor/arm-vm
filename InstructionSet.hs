{-# LANGUAGE DeriveGeneric #-}
module InstructionSet
( MInt
, Reg(..)
, Imm(..)
, Op(..)
) where
import GHC.Generics
import Data.Int

-- | The default native integer of the emulated machine
type MInt = Int32

-- | A numbered register, e.g. %r0
newtype Reg = Reg MInt deriving (Show, Eq)

-- | A immediate number, used as an argument in some ops
newtype Imm = Imm MInt deriving (Show, Eq)

-- | A machine operation/instruction
data Op = ADD Reg Reg Reg
        | SUB Reg Reg Reg
        | MUL Reg Reg Reg
        | CMP Reg Reg Reg
        | JGT Reg Imm
        | LDI Reg Imm
  deriving (Show, Eq, Generic)
