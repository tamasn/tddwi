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

Functor Expr where
  map func (Val x) = Val (func x)
  map func (Add x y) = Add (map func x) (map func y)
  map func (Sub x y) = Sub (map func x) (map func y)
  map func (Mul x y) = Mul (map func x) (map func y)
  map func (Div x y) = Div (map func x) (map func y)
  map func (Abs' x) = Abs' (map func x) 

data Vect : Nat -> Type -> Type where
  Nil     : Vect Z a
  (::)    : (x : a) -> (xs : Vect k a) -> Vect (S k) a

Eq a => Eq (Vect n a) where
  (==) [] [] = True
  (==) (x :: xs) (y :: ys) = x == y && xs == ys

Foldable (Vect n) where
  foldr func init [] = init
  foldr func init (x :: xs) = 
    let rest = foldr func init xs in
    func x rest 
