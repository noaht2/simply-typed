{-# LANGUAGE ParallelListComp, InstanceSigs #-}
module Parsing where
import Types
import Control.Monad
import Control.Applicative

newtype Parser a = Parser (String -> [(a, String)])

apply :: Parser a -> String -> [(a, String)]
apply (Parser p) s = p s

parse :: Parser a -> String -> a
parse p = fst . head . apply p

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  -- fmap takes a function as its first argument that changes the output of a parse and
  -- returns a Parser that does that manipulation after parsing according to fmap’s
  -- second argument.
  fmap f (Parser p) = Parser (\s -> map
                               (\(x, s') -> (f x, s'))
                               (p s))

instance Applicative Parser where
  pure :: a -> Parser a
  -- pure returns a Parser that returns pure’s argument and moves on.
  pure x = Parser (\s -> [(x, s)])
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  -- (<*>) uses the parse-function that is its first operand to modify the
  -- parse-function that is its second argument based on the text to be parsed.
  -- (<*>) probably doesn’t satisfy the laws of Applicative.
  fp <*> p = Parser (\s -> [(y, s''')
                           | (f, s') <- apply fp s,
                             (x, s'') <- apply p s',
                             (y, s''') <- apply (pure (f x)) s''])

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= q = Parser (\s -> [(y, s'')
                          | (x, s') <- apply p s,
                            (y, s'') <- apply (q x) s'])

instance Alternative Parser where
  empty :: Parser a
  empty = Parser (\s -> [])
  (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = Parser f
    where f s = let ps = apply p s in
                  if null ps then apply q s
                  else ps
  some :: Parser a -> Parser [a]
  some p = do {x <- p; xs <- many p; return (x:xs)} <|> none
  many :: Parser a -> Parser [a]
  many = some_or_none . some

none :: Parser [a]
none = return []

some_or_none :: Parser [a] -> Parser [a]
some_or_none p = p <|> none

instance MonadPlus Parser where
  mzero = empty
  mplus = (<|>)

getc :: Parser Char
getc = Parser f
  where f [] = []
        f (c:cs) = [(c,cs)]

-- guard :: Bool -> Parser ()
-- guard True = return ()
-- guard False = fail2parse

sat :: (Char -> Bool) -> Parser Char
sat p = do {c <- getc;
            guard (p c);
            return c}

char :: Char -> Parser ()
char x = do {c <- sat (==x);
             return ()}

alphabet = "QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm'′″‴⁗+×Θ"

var :: Parser Var
var = do {v <- many (sat (flip elem alphabet)); return (Var v)}

app :: Parser App
app = do {char '(';
          m <- expr;
          char ' ';
          n <- expr;
          char ')';
          return (App m n)}

abstr :: Parser Abstr
abstr = do {char '(';
            char 'λ';
            v <- var;
            char '.';
            m <- expr;
            char ')';
            return (Abstr v m)}

expr :: Parser Expr
expr = abstr <|> app <|> var
