{-# LANGUAGE ParallelListComp, InstanceSigs, RecursiveDo #-}
module Parsing (expr, parse) where
import Types
import Control.Monad
import Control.Applicative

done = return ()

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
  p <*> q = Parser (\s -> [(y, s''')
                           | (f, s') <- apply p s,
                             (x, s'') <- apply q s',
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

instance MonadFail Parser where
  fail _ = mzero

getc :: Parser Char
getc = Parser f
  where f [] = []
        f (c:cs) = [(c,cs)]

sat :: (Char -> Bool) -> Parser Char
sat p = do {c <- getc;
            guard (p c);
            return c}

char :: Char -> Parser ()
char x = do {c <- sat (==x);
             return ()}

string :: String -> Parser ()
string (c:cs) = char c >> string cs
string "" = done

alphabet = "QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm'′″‴⁗+×Θ"

base :: Parser SimpleType
base = do {char 'B';
           return (Left Base)}

func_type :: Parser SimpleType
func_type = do {char '(';
                param_type <- simple_type;
                char '→';
                result_type <- simple_type;
                char ')';
                return $ Right $ Left $ param_type `To` result_type}

product_type :: Parser SimpleType
product_type = do {char '(';
                   first_type <- simple_type;
                   char '×';
                   second_type <- simple_type;
                   char ')';
                   return $ Right $ Right $ Left $ first_type `Prod` second_type}

sum_type :: Parser SimpleType
sum_type = do {char '(';
               first_type <- simple_type;
               char '+';
               second_type <- simple_type;
               char ')';
               return $ Right $ Right $ Right $ first_type `Sum` second_type}

simple_type :: Parser SimpleType
simple_type = base <|> func_type <|> product_type <|> sum_type

first_parser :: Parser Expr
first_parser = do {string "fst";
                   return $ Left Fst}

second_parser :: Parser Expr
second_parser = do {string "snd";
                    return $ Left Snd}

var :: Parser Expr
var = do {char '(';
          v <- many (sat (flip elem alphabet));
          char ':';
          t <- simple_type;
          char ')';
          return $ Right $ Left $ Var v t}

abstr :: Parser Expr
abstr = do {string "(λ";
            (Right (Left v)) <- var;
            char '.';
            (Right m) <- expr;
            char ')';
            return $ Right $ Right $ Left $ Abstr v m}

app :: Parser Expr
app = do {char '(';
          m <- expr;
          char ' ';
          (Right n) <- expr;
          char ')';
          return $ Right $ Right $ Right $ Left $ App m n}

pair :: Parser Expr
pair = do {char '⟨';
           (Right m) <- expr;
           char ',';
           (Right n) <- expr;
           char '⟩';
           return $ Right $ Right $ Right $ Right $ Left $ Pair m n}

left_union :: Parser Expr
left_union = do {string "(Left ";
                 (Right m) <- expr;
                 char ' ';
                 t <- simple_type;
                 char ')';
                 return $ Right $ Right $ Right $ Right $ Right $ Union (get_type m `Sum` t) True m}

right_union :: Parser Expr
right_union = do {string "(Right ";
                  t <- simple_type;
                  char ' ';
                  (Right m) <- expr;
                  char ')';
                  return $ Right $ Right $ Right $ Right $ Right $ Union (t `Sum` get_type m) False m}

expr :: Parser Expr
expr = pair <|> abstr <|> app <|> var <|> first_parser <|> second_parser <|> left_union <|> right_union
