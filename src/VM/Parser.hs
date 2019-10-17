{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module VM.Parser
( parseProgram
, opParser
, parseAssembly
) where
import VM.Internal.MonadParser
import VM.InstructionSet

import GHC.Generics
import Data.Char(isDigit, isSpace, toLower)
import Control.Monad(MonadPlus, guard)
import Control.Monad.Fail(MonadFail)

--parseProgram :: String -> ParseResult [Op]
parseProgram xs = do
    parsedOps <- parseAssembly xs
    guard (all (all isSpace) $ fmap snd parsedOps) <|> fail "encountered unconsumed input"
    return (map fst parsedOps)

parseAssembly :: (MonadPlus m, MonadFail m) => String -> m [(Op, String)]
parseAssembly = traverse id
    . zipWith runParser parsers
    . filter (not . all isSpace)
    . fmap (takeWhile (/= ';'))
    . lines
    . fmap toLower
    where parsers = map (\n -> opParser <|> fail ("error: syntax error on statement " ++ show n)) [1..]


opParseLines :: (MonadPlus m, MonadFail m) => String -> m [(Op, String)]
opParseLines = traverse (runParser opParser)
             . filter (not . all isSpace)
             . fmap (takeWhile (/= ';'))
             . lines
             . fmap toLower

opParser :: (MonadPlus m, MonadFail m) => ParserT String m Op
opParser = fmap to . argParser . from $ (undefined :: Op)

register :: (Stream s, MonadPlus m, MonadFail m) => ParserT s m Reg
register = do
  whiteSpace
  string "%r"
  number >>= (pure . Reg)

-- | Defines the usual arithmetic operations, for use in the constExpr parser
data Operator a = Operator { precondition :: (a -> a -> Bool), op :: (a -> a -> a), symbol :: Char, precedence :: Int }
-- | Addition is a total binary op
plus :: Num a => Operator a
plus = Operator (const . const True) (+) '+' 1
-- | Substraction is a total binary op
minus :: Num a => Operator a
minus = Operator (const . const True) (-) '-' 1
-- | Multiplication is a total binary op
multiplication :: Num a => Operator a
multiplication = Operator (const . const True) (*) '*' 2
-- | Division is a binary op defined for all numbers except divide by zero
division :: Integral a => Operator a
division = Operator (\x y -> y /= 0) div '/' 2
-- | Integer exponentiation is only defined for positive exponents.
exponentiation :: Integral a => Operator a
exponentiation = Operator (\x y -> y >= 0) (^) '^' 3


ops :: Integral a => [Operator a]
ops = [plus, minus, multiplication, division, exponentiation]

constExpr'' :: (Stream s, MonadPlus m, MonadFail m, Integral a, Read a)
            => Int -> a -> ParserT s m a
constExpr'' prec lhs = choice [operator, constExpr' prec, return lhs]
    where
        op o = char (symbol o) >> return o
        operator = do
            (Operator c f s p) <- choice . map op $ filter ((> prec) . precedence) ops
            rhs <- constExpr' p
            guard (c lhs rhs)
            constExpr'' prec (f lhs rhs)

constExpr' :: (Stream s, MonadPlus m, MonadFail m, Read a, Integral a)
           => Int -> ParserT s m a
constExpr' prec = (choice [unsigned, composite] >>= constExpr'' prec)
    where composite = between (char '(') (char ')') constExpr


constExpr :: (Stream s, MonadPlus m, MonadFail m, Integral a, Read a) => ParserT s m a
constExpr = constExpr' 0 <|> (peekChar '-' >> constExpr'' 0 0)


immediate :: (MonadPlus m, MonadFail m) => ParserT String m Imm
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

left :: Either a b -> a
left = undefined

right :: Either a b -> b
right = undefined

class AssemblyArgParser a where
  argParser :: (MonadPlus m, MonadFail m) => a -> ParserT String m a

instance AssemblyArgParser Imm where
  argParser _ = immediate

instance AssemblyArgParser Reg where
  argParser _ = register

instance (AssemblyArgParser p, AssemblyArgParser q) => AssemblyArgParser (Either p q) where
  argParser x = p <|> q
    where
        p = fmap Left (argParser $ undefined `asTypeOf` left x)
        q = fmap Right (argParser $ undefined `asTypeOf` right x)

instance (Constructor c, AssemblyArgParser (f p)) => AssemblyArgParser (C1 c f p) where
  argParser x = do
    string (fmap toLower $ conName x) <|> fail "expected op"
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
