module Core where
import Types
import Parsing

free :: Var -> SimplyTypedTerm -> Bool
free x (Left v) = x == v
free x (Right (Left (Abstr y m))) = (x /= y) && free x m
free x (Right (Right (Left (App (Right m) n)))) = free x m || free x n
free x (Right (Right (Left (App (Left _) n)))) = free x n
free x (Right (Right (Right (Left (Pair m n))))) = free x m || free x n
free x (Right (Right (Right (Right (Union _ _ m))))) = free x m

beta_reduce :: SimplyTypedTerm -> Var -> SimplyTypedTerm -> SimplyTypedTerm
-- M[N /x] = beta M x N
beta_reduce (Left v) x n
  | v == x = n
  | otherwise = Left v
beta_reduce (Right (Right (Left (App (Right m) p)))) x n = Right $ Right $ Left $ App (Right (beta_reduce m x n)) (beta_reduce p x n)
beta_reduce unchanged@(Right (Left (Abstr y m))) x n
  | y == x = unchanged
  | (y /= x) && not (free y n) = Right $ Left $ Abstr y (beta_reduce m x n)
beta_reduce m _ _ = m

beta :: Expr -> Expr
beta v@(Right (Left _)) = v
beta (Right (Right (Left (Abstr v m)))) = Right $ Right $ Left $ Abstr v m'
  where Right m' = beta $ Right m
beta (Right (Right (Right (Left (App (Right (Right (Left (Abstr v m)))) n))))) = Right $ beta_reduce m v n
beta (Right (Right (Right (Left (App (Left Fst) (Right (Right (Right (Left p))))))))) = beta $ Right $ first_item p
beta (Right (Right (Right (Left (App (Left Snd) (Right (Right (Right (Left p))))))))) = beta $ Right $ second_item p
beta (Right (Right (Right (Left (App m n))))) = Right $ Right $ Right $ Left $ App (beta m) n'
  where Right n' = beta $ Right n
beta (Left Fst) = Left Fst
beta (Left Snd) = Left Snd
beta (Right (Right (Right (Right (Left (Pair m n)))))) = Right $ Right $ Right $ Right $ Left $ Pair m' n'
  where Right m' = beta $ Right m
        Right n' = beta $ Right n
beta (Right (Right (Right (Right (Right (Union t b m)))))) = Right $ Right $ Right $ Right $ Right $ Union t b m'
  where Right m' = beta $ Right m

eta :: Expr -> Expr
eta v@(Right (Left _)) = v
eta (Right (Right (Right (Left (App m n))))) = Right $ Right $ Right $ Left $ App (eta m) n'
  where Right n' = eta (Right n)
eta (Left Fst) = Left Fst
eta (Left Snd) = Left Snd
eta (Right (Right (Left (Abstr x (Right (Right (Left (App m@(Right _) y))))))))
  | (Left x) == y = m
eta (Right (Right (Left (Abstr v m)))) = Right $ Right $ Left $ Abstr v m'
  where Right m' = eta $ Right m
eta (Right (Right (Right (Right (Left (Pair m n)))))) = Right $ Right $ Right $ Right $ Left $ Pair m' n'
  where Right m' = eta $ Right m
        Right n' = eta $ Right n
eta (Right (Right (Right (Right (Right (Union t b m)))))) = Right $ Right $ Right $ Right $ Right $ Union t b m'
  where Right m' = eta $ Right m

simplify_method :: (Expr -> Expr) -> Expr -> Expr
simplify_method pass e = if e == x then e else simplify_method pass x
  where x = pass e

simplify :: Expr -> Expr
simplify = simplify_method eta . simplify_method beta
