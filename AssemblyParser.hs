{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
-- Temporarily
{-# LANGUAGE NoMonomorphismRestriction #-}
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

opParser :: (MonadPlus m) => ParserT String m Op
opParser = fmap to . argParser . from $ (undefined :: Op)

register :: (Stream s, MonadPlus m) => ParserT s m Reg
register = do
  whiteSpace
  string "%r"
  number >>= (pure . Reg)

-- | Defines the usual arithmetic operations, for use in the constExpr parser
data Operator a = Operator { precondition :: (a -> a -> Bool), op :: (a -> a -> a), symbol :: Char, precedence :: Int }
-- | Addition is a total binary op
plus = Operator (const . const True) (+) '+' 1
-- | Substraction is a total binary op
minus = Operator (const . const True) (-) '-' 1
-- | Multiplication is a total binary op
multiplication = Operator (const . const True) (*) '*' 2
-- | Division is a binary op defined for all numbers except divide by zero
division = Operator (\x y -> y /= 0) div '/' 2
-- | Integer exponentiation is only defined for positive exponents.
exponentiation = Operator (\x y -> y >= 0) (^) '^' 3


ops :: Integral a => [Operator a]
ops = [plus, minus, multiplication, division, exponentiation]

constExpr'' :: (Stream s, MonadPlus m, Integral a, Read a)
            => Int -> a -> ParserT s m a
constExpr'' prec lhs = choice [operator, constExpr' prec, return lhs]
    where
        op o = char (symbol o) >> return o
        operator = do
            (Operator c f s p) <- choice . map op $ filter ((> prec) . precedence) ops
            rhs <- constExpr' p
            guard (c lhs rhs)
            constExpr'' prec (f lhs rhs)

constExpr' :: (Stream s, MonadPlus m, Read a, Integral a)
           => Int -> ParserT s m a
constExpr' prec = (choice [unsigned, composite] >>= constExpr'' prec)
    where composite = between (char '(') (char ')') constExpr


constExpr :: (Stream s, MonadPlus m, Integral a, Read a) => ParserT s m a
constExpr = constExpr' 0 <|> (peekChar '-' >> constExpr'' 0 0)


immediate :: (MonadPlus m) => ParserT String m Imm
immediate = choice [number, expr] >>= (pure . Imm)
    where expr = char '=' >> preprocess (filter $ not . isSpace) >> constExpr

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
  argParser :: (MonadPlus m) => a -> ParserT String m a

instance AssemblyArgParser Imm where
  argParser _ = immediate

instance AssemblyArgParser Reg where
  argParser _ = register

instance (Constructor c, AssemblyArgParser (f p)) => AssemblyArgParser (C1 c f p) where
  argParser x = do
    string (fmap toLower $ conName x)
    fmap M1 $ argParser (undefined `asTypeOf` unM1 x)

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
