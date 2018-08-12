data Expr num = Val num
              | Add (Expr num) (Expr num)
              | Sub (Expr num) (Expr num)
              | Mul (Expr num) (Expr num)
              | Div (Expr num) (Expr num)
              | Abs' (Expr num)

eval : (Neg num, Integral num, Abs num) => Expr num -> num
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Abs' x) = abs (eval x)

Num a => Num (Expr a) where
  (+) = Add
  (*) = Mul
  fromInteger = Val . fromInteger
  
Show a => Show (Expr a) where
  show (Val x) = show x
  show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (Sub x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
  show (Mul x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
  show (Div x y) = "(" ++ show x ++ " / " ++ show y ++ ")"
  show (Abs' x) = "abs(" ++ show x ++ ")"

(Eq a, Neg a, Integral a, Abs a) => Eq (Expr a) where
  (==) x y = eval x == eval y

(Neg a, Integral a, Abs a) => Cast (Expr a) a where
  cast orig = eval orig 
