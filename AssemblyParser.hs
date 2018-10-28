{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
module AssemblyParser
( parseProgram
, opParser
) where
import Parser
import GHC.Generics
import InstructionSet
import Data.Char(isDigit, isSpace, toLower)
import Control.Monad(MonadPlus, guard)

parseProgram :: String -> Maybe [Op]
parseProgram xs = do
    parsedOps <- opParseLines xs
    guard (all (all isSpace) $ map snd parsedOps)
    return (map fst parsedOps)


opParseLines :: String -> Maybe [(Op, String)]
opParseLines = traverse (runParser opParser)
             . filter (not . all isSpace)
             . map (takeWhile (/= ';'))
             . lines
             . map toLower

opParser :: (Stream s, MonadPlus m) => ParserT s m Op
opParser = fmap to . argParser . from $ (undefined :: Op)

register :: (Stream s, MonadPlus m) => ParserT s m Reg
register = do
  whiteSpace
  string "%r"
  number >>= (pure . Reg)

immediate :: (Stream s, MonadPlus m) => ParserT s m Imm
immediate = number >>= (pure . Imm)

-- | Lazy functions used solely for type-trickery
sumL :: ((:+:) f g p) -> (f p)
sumL = undefined

sumR :: ((:+:) f g p) -> (g p)
sumR = undefined

prodL :: ((:*:) f g p) -> (f p)
prodL = undefined

prodR :: ((:*:) f g p) -> (g p)
prodR = undefined

class AssemblyArgParser a where
  argParser :: (Stream s, MonadPlus m) => a -> ParserT s m a

instance AssemblyArgParser Imm where
  argParser _ = immediate

instance AssemblyArgParser Reg where
  argParser _ = register

instance (Constructor c, AssemblyArgParser (f p)) => AssemblyArgParser (C1 c f p) where
  argParser x = do
    string (fmap toLower $ conName x)
    fmap M1 $ argParser (error "0" `asTypeOf` unM1 x)

instance AssemblyArgParser (f p) => AssemblyArgParser (D1 c f p) where
  argParser x = fmap M1 $ argParser (undefined `asTypeOf` unM1 x)

instance AssemblyArgParser (f p) => AssemblyArgParser (S1 c f p) where
  argParser x = fmap M1 $ argParser (undefined `asTypeOf` unM1 x)

instance AssemblyArgParser c => AssemblyArgParser (K1 i c p) where
  argParser x = fmap K1 $ argParser (undefined `asTypeOf` unK1 x)

instance (AssemblyArgParser (f p), AssemblyArgParser (g p)) => AssemblyArgParser ((:+:) f g p) where
  argParser x = fmap L1 (argParser $ sumL x) <|> fmap R1 (argParser $ sumR x)

instance (AssemblyArgParser (f p), AssemblyArgParser (g p)) =>  AssemblyArgParser ((:*:) f g p) where
  argParser ab = do
    a' <- argParser (undefined `asTypeOf` prodL ab)
    whiteSpace
    char ','
    whiteSpace
    b' <- argParser (undefined `asTypeOf` prodR ab)
    return (a' :*: b')
