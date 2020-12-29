module Types where

data Var = Var {name :: String, var_type :: SimpleType}

data Abstr = Abstr {param :: Var, body :: SimplyTypedTerm}
data App = App {func :: Expr, arg :: SimplyTypedTerm}
data Pair = Pair {first_item :: SimplyTypedTerm, second_item :: SimplyTypedTerm}
data Union = Union {union_type :: SimpleType, tag :: Bool, value :: SimplyTypedTerm}
data Case = Case {union :: Union, left_func :: Abstr, right_func :: Abstr}

data Base = Base
data Func = To {param_type :: SimpleType, result_type :: SimpleType}
data Product = Prod {first :: SimpleType, second :: SimpleType}
data Sum = SimpleType `Sum` SimpleType

data Selector = Fst | Snd

type SimpleType = Either Base (Either Func (Either Product Sum))
type SimplyTypedTerm = Either Var (Either Abstr (Either App (Either Pair Union)))
type Expr = Either Selector SimplyTypedTerm

data Environment = Empty | Environment `With` SimplyTypedTerm

get_type :: SimplyTypedTerm -> SimpleType
get_type (Left (Var _ t)) = t
get_type (Right (Left (Abstr (Var _ t) m))) = Right (Left (t `To` (get_type m)))
get_type (Right (Right (Left (App (Left Fst) n)))) = first t
  where Right (Right (Left t)) = get_type n
get_type (Right (Right (Left (App (Left Snd) n)))) = second t
  where Right (Right (Left t)) = get_type n
get_type (Right (Right (Left (App (Right m) n)))) = result_type t
  where Right (Left t) = get_type m
get_type (Right (Right (Right (Left (Pair m n))))) = Right $ Right $ Left $ get_type m `Prod` get_type n
get_type (Right (Right (Right (Right (Union t True m))))) = Right $ Right $ Right $ t `Sum` get_type m
get_type (Right (Right (Right (Right (Union t False m))))) = Right $ Right $ Right $ get_type m `Sum` t

instance Eq Var where
  Var x t == Var y u = x == y && t == u

instance Eq Abstr where
  Abstr x m == Abstr y n = x == y && m == n

instance Eq App where
  App m n == App o p = m == o && n == p

instance Eq Pair where
  Pair a b == Pair c d = (a,b) == (c,d)

instance Eq Union where
  Union a t x == Union b u y = (a,t,x) == (b,u,y)

instance Eq Selector where
  Fst == Fst = True
  Snd == Snd = True

instance Eq Base where
  Base == Base = True
  -- _ == _ = False

instance Eq Func where
  a `To` b == c `To` d = (a,b) == (c,d)
  -- _ == _ = False

instance Eq Product where
  a `Prod` b == c `Prod` d = (a,b) == (c,d)
  -- _ == _ = False

instance Eq Sum where
  a `Sum` b == c `Sum` d = (a,b) == (c,d)

display_type :: SimpleType -> String
display_type (Left Base) = "B"
display_type (Right (Left (a `To` b))) = "(" ++ display_type a ++ "→" ++ display_type b ++ ")"
display_type (Right (Right (Left (a `Prod` b)))) = "(" ++ display_type a ++ "×" ++ display_type b ++ ")"
display_type (Right (Right (Right (a `Sum` b)))) = "(" ++ display_type a ++ "+" ++ display_type b ++ ")"

display :: Expr -> String
display (Right (Left (Var v t))) = "(" ++ v ++ ":" ++ display_type t ++ ")"
display (Right (Right (Left (Abstr v e)))) = "(λ" ++ display (Right (Left v)) ++ "." ++ display (Right e) ++ ")"
display (Right (Right (Right (Left (App f e))))) = "(" ++ display f ++ " " ++ display (Right e) ++ ")"
display (Right (Right (Right (Right (Left (Pair m n)))))) = "⟨" ++ display (Right m) ++ "," ++ display (Right n) ++ "⟩"
display (Left Fst) = "fst"
display (Left Snd) = "snd"
display (Right (Right (Right (Right (Right (Union t True m)))))) = "(Left " ++ display (Right m) ++ " " ++ display_type t ++ ")"
display (Right (Right (Right (Right (Right (Union t False m)))))) = "(Right " ++ display_type t ++ " " ++ display (Right m) ++ ")"
