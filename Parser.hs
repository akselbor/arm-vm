{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
module Parser
( Parser
, ParserT
, pattern ParserT
, Stream(..)
, runParser
, choice
, between
, peek
, peekChar
, item
, sat
, char
, string
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
import Control.Monad.State
import Data.Char(isSpace, isDigit)
import Data.Maybe(maybe)
import Text.Read(readMaybe)

-- | To define Stream instances
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy.Char8
import qualified Data.List


-- | A Monadic parser. *Heavily* based on http://www.cs.nott.ac.uk/~pszgmh/pearl.pdf
-- | Previous ParserT definition was equivalent to StateT, so now it's simply a StateT wrapper
newtype ParserT s m a = ParserT_ (StateT s m a)
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadTrans)

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
    unconsM x = maybe mzero return (uncons x)


instance Stream String where
    uncons = Data.List.uncons

instance Stream Data.ByteString.Char8.ByteString where
    uncons = Data.ByteString.Char8.uncons

instance Stream Data.ByteString.Lazy.Char8.ByteString where
    uncons = Data.ByteString.Lazy.Char8.uncons

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
    guard (p x)
    return x

-- | Parse the given character, and fail otherwise
char :: (Stream s, MonadPlus m)
     => Char -> ParserT s m Char
char = sat . (==)

-- | Parse the given character without actually consuming input, or fail otherwise
peekChar :: (Stream s, MonadPlus m)
         => Char -> ParserT s m Char
peekChar c = do
    c' <- peek
    guard (c' == c)
    return c'

-- | Parse the given string, and fail otherwise
string :: (Stream s, MonadPlus m)
       => String -> ParserT s m String
string xs = traverse char xs <|> err xs
  where
    err xs = do
      ys <- replicateM (length xs) item
      fail ("expected \"" ++ xs ++ "\", got " ++ "\"" ++ ys ++ "\"")

-- | Attempt all parsers in order. Return the first successful parser
choice :: (Stream s, MonadPlus m)
       => [ParserT s m a] -> ParserT s m a
choice = foldr (<|>) mzero

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

-- | Parse a unsigned number
unsigned :: (Stream s, MonadPlus m, Num a, Read a)
         => ParserT s m a
unsigned = do
    xs <- some (sat isDigit) <|> fail "no digits to parse"
    (Just num) <- (return $ readMaybe xs) <|> fail ("could not parse \"" ++ xs ++ "\" into a number")
    return num

-- | Parse a number
number :: (Stream s, MonadPlus m, Num a, Read a)
       => ParserT s m a
number = do
    negative <- (char '-' >> return True) <|> return False
    num <- unsigned
    if negative then return (-num) else return num

-- | Preprocess a string
--preprocess :: (Stream s, MonadPlus m) => (s -> s) -> ParserT s m s
preprocess f = ParserT $ \xs -> return ((), f xs)
