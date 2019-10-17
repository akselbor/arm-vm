{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
module VM.Internal.MonadParser
( Parser
, ParserT
, ParseResult(..)
, pattern ParserT
, Stream(..)
, expectedGot
, expected
, runParser
, choice
, between
, peek
, peekChar
, item
, sat
, char
, string
, word
, unsigned
, number
, whiteSpace
, readMaybe
, joinT
, preprocess
, module Control.Applicative
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fail(MonadFail)
import Control.Monad.State
import Data.Char(isSpace, isDigit)
import Data.Maybe(maybe)
import Text.Read(readMaybe)

-- | To define Stream instances
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy.Char8
import qualified Data.List

import qualified Control.Monad.Fail


-- | A Monadic parser. *Heavily* based on http://www.cs.nott.ac.uk/~pszgmh/pearl.pdf
-- | Previous ParserT definition was equivalent to StateT, so now it's simply a StateT wrapper
newtype ParserT s m a = ParserT_ (StateT s m a)
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadTrans, MonadFail)

data ParseResult a = Failure String | Result a
    deriving (Eq, Show)

instance Alternative ParseResult where
    lhs@(Result _) <|> _ = lhs
    _ <|> rhs = rhs
    empty = Failure "empty"

instance Functor ParseResult where
    fmap f (Failure msg) = Failure msg
    fmap f (Result x) = Result (f x)

instance Applicative ParseResult where
    pure = Result
    (Failure msg) <*> x = Failure msg
    f <*> (Failure msg) = Failure msg
    (Result f) <*> (Result x) = Result (f x)


instance Monad ParseResult where
    return = Result
    (Failure x) >>= f = Failure x
    (Result x) >>= f = f x
    fail = Failure

instance MonadPlus ParseResult where
    mzero = fail "mzero"
    mplus = (<|>)

instance MonadFail ParseResult where
    fail _ = mzero

type Parser' a = ParserT String ParseResult a

type Parser a = ParserT String Maybe a

-- | Pattern synonym which allows ParserT to be used as if it
-- | hadn't been defined in terms of StateT:
-- | ParserT s m a = ParserT (s -> m (a, s))
pattern ParserT p = ParserT_ (StateT p)

runParser :: ParserT s m a -> s -> m (a, s)
runParser (ParserT p) = p


joinT :: Monad m
      => ParserT s m (m a) -> ParserT s m a
joinT p = ParserT $ \xs -> do
        (x, ys) <- runParser p xs
        y <- x
        return (y, ys)


-- | A typeclass for string-like structures which can contain a head and a tail
-- | Minimal complete definition: uncons | unconsM
class Stream a where
    uncons :: a -> Maybe (Char, a)
    uncons = unconsM

    unconsM :: MonadPlus m => a -> m (Char, a)
    unconsM x = maybe (fail "unexpected eof") return (uncons x)


instance Stream String where
    uncons = Data.List.uncons

instance Stream Data.ByteString.Char8.ByteString where
    uncons = Data.ByteString.Char8.uncons

instance Stream Data.ByteString.Lazy.Char8.ByteString where
    uncons = Data.ByteString.Lazy.Char8.uncons

expectedGot ex act = fail $ "expected '" ++ ex ++ "', got '" ++ act ++ "'"
expected ex = word >>= \x -> fail $ "expected " ++ ex ++ ", got " ++ x

-- | Parse a character. Will fail on empty strings
item :: (Stream s, MonadPlus m)
     => ParserT s m Char
item = ParserT unconsM

-- | Extract the first character without consuming it
peek :: (Stream s, MonadPlus m)
     => ParserT s m Char
peek = ParserT peekM
    where peekM x = (fmap . fmap) (const x) (unconsM x)


-- | Parse a character satisfying the predicate
sat :: (Stream s, MonadPlus m)
    => (Char -> Bool) -> ParserT s m Char
sat p = do
    x <- item
    guard (p x) <|> fail ("unexpected token '" ++ [x] ++ "'")
    return x

-- | Parse the given character, and fail otherwise
char :: (Stream s, MonadPlus m)
     => Char -> ParserT s m Char
char c = sat (== c) <|> (fmap (:[]) item >>= expectedGot [c])

-- | Parse the given character without actually consuming input, or fail otherwise
peekChar :: (Stream s, MonadPlus m)
         => Char -> ParserT s m Char
peekChar c = do
    c' <- peek
    guard (c' == c) <|> expectedGot [c] [c']
    return c'

-- | Parse the given string, and fail otherwise
string :: (Stream s, MonadPlus m)
       => String -> ParserT s m String
string xs = traverse char xs <|> err xs
  where
    err xs = choice [replicateM (length xs) item, many item] >>= expectedGot xs

word :: (Stream s, MonadPlus m)
     => ParserT s m String
word = many (sat $ not . isSpace)

-- | Attempt all parsers in order. Return the first successful parser
choice :: (Stream s, MonadPlus m)
       => [ParserT s m a] -> ParserT s m a
choice = foldr (<|>) (fail "exhausted choices")

between :: (Stream s, MonadPlus m)
        => ParserT s m open -> ParserT s m close -> ParserT s m a -> ParserT s m a
between open close p = do
    open
    x <- p
    close
    return x

-- | Parse whitespace
whiteSpace :: (Stream s, MonadPlus m)
           => ParserT s m String
whiteSpace = many (sat isSpace)

decimal :: (Stream s, MonadPlus m) => ParserT s m String
decimal = some (sat isDigit)

hexadecimal :: (Stream s, MonadPlus m) => ParserT s m String
hexadecimal = do
    string "0x"
    digits <- some $ sat isDigit <|> choice (map char $ ['A'..'Z'] ++ ['a'..'z'])
    return $ '0' : 'x' : digits


-- | Parse a unsigned number
unsigned :: (Stream s, MonadPlus m, MonadFail m, Num a, Read a)
         => ParserT s m a
unsigned = do
    xs <- hexadecimal <|> decimal <|> fail "no digits to parse"
    (Just num) <- (return $ readMaybe xs) <|> fail ("could not parse \"" ++ xs ++ "\" into a number")
    return num

-- | Parse a number
number :: (Stream s, MonadPlus m, MonadFail m, Num a, Read a)
       => ParserT s m a
number = do
    negative <- (char '-' >> return True) <|> return False
    num <- unsigned
    if negative then return (-num) else return num

-- | Preprocess a string
--preprocess :: (Stream s, MonadPlus m) => (s -> s) -> ParserT s m s
preprocess f = ParserT $ \xs -> return ((), f xs)
