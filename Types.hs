module Types where

data Expr = Var {name :: String, var_type :: Type}
          | Abstr {param :: String, param_type :: Type, body :: Expr}
          | App {func :: Expr, arg :: Expr}
          | Pair {first_item :: Expr, second_item :: Expr}
          | Fst
          | Snd
          | Union {other_type :: Type, tag :: Bool, value :: Expr}
          | Case {union :: Expr, left_func :: Expr, right_func :: Expr}
          deriving (Eq, Show)

apply :: (Expr -> Expr) -> Expr -> Expr
apply f v@(Var _ _) = v
apply f (Abstr v t m) = Abstr v t (f m)
apply f (App m n) = App (f m) (f n)
apply f (Pair m n) = Pair (f m) (f n)
apply f (Union o t v) = Union o t (f v)
apply f (Case u l r) = Case (f u) l r
apply f Fst = Fst
apply f Snd = Snd

data Type = Base
          | To {from :: Type, to :: Type}
          | Prod {multiplicand:: Type, multiplier :: Type}
          | Sum {augend :: Type, addend  :: Type}
          deriving (Eq, Show)

get_type :: Expr -> Type
get_type (Var _ t) = t
get_type (Abstr _ t m) = t `To` get_type m
get_type (App m _) = to $ get_type m
get_type (Pair m n) = get_type m `Prod` get_type n
get_type (Union t b m) = if b then get_type m `Sum` t else t `Sum` get_type m
get_type (Case _ l _) = to $ get_type l

well_typed :: Expr -> Bool
well_typed (App Fst m) = case get_type m of
                           (_ `Prod` _) -> True
                           _ -> False
well_typed (App Snd m) = case get_type m of
                           (_ `Prod` _) -> True
                           _ -> False
well_typed (App m n) = case get_type m of
                         (t `To` _) -> t == get_type n
                         _ -> False
well_typed (Case u l r) = case get_type u of
                            (t1 `Sum` t2) -> case get_type l of
                                               (t3 `To` t4) -> case get_type r of
                                                                  (t5 `To` t6) -> ((t1 == t3 && t2 == t5) || (t1 == t5 && t2 == t3)) && t4 == t6
                                                                  _ -> False
                                               _ -> False
                            _ -> False
well_typed (Abstr _ _ m) = well_typed m
well_typed (Pair m n) = well_typed m && well_typed n
well_typed (Union _ _ v) = well_typed v
well_typed Fst = True
well_typed Snd = True
well_typed (Var _ _) = True

display_type :: Type -> String
display_type Base = "B"
display_type (a `To` b) = "(" ++ display_type a ++ "→" ++ display_type b ++ ")"
display_type (a `Prod` b) = "(" ++ display_type a ++ "×" ++ display_type b ++ ")"
display_type (a `Sum` b) = "(" ++ display_type a ++ "+" ++ display_type b ++ ")"

display :: Expr -> String
display (Var v t) = "(" ++ v ++ ":" ++ display_type t ++ ")"
display (Abstr v t e) = "(λ" ++ display (Var v t) ++ "." ++ display e ++ ")"
display (App f e) = "(" ++ display f ++ " " ++ display e ++ ")"
display (Pair m n) = "⟨" ++ display m ++ "," ++ display n ++ "⟩"
display Fst = "fst"
display Snd = "snd"
display (Union t True m) = "(Left " ++ display m ++ " " ++ display_type t ++ ")"
display (Union t False m) = "(Right " ++ display_type t ++ " " ++ display m ++ ")"
display (Case u l r) = "(case "
  ++ display u
  ++ " of "
  ++ display l
  ++ " or "
  ++ display r
  ++ ")"
