module Types where

data Expr = Var {varname :: String} | Abstr {param :: Var, body :: Expr} | App {func :: Expr, arg :: Expr} | Special {value :: Expr, special_name :: Var}
type Var = Expr
type Abstr = Expr
type App = Expr
type Special = Expr

-- data Var = Var {name :: String}
-- data Abstr = Abstr {param :: Var, body :: Expr}
-- data App = App {func :: Expr, arg :: Expr}

-- type Expr = Either Var (Either Abstr App)
