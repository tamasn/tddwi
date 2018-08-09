data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mult Expr Expr

evaluate : Expr -> Int
evaluate (Val x) = x 
evaluate (Add x y) = (evaluate x) + (evaluate y)
evaluate (Sub x y) = (evaluate x) - (evaluate y)
evaluate (Mult x y) = (evaluate x) * (evaluate y)

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing y = y
maxMaybe max@ (Just x) Nothing = max
maxMaybe lh@ (Just x) rh@ (Just y) = case compare x y of
                                  LT => rh
                                  EQ => lh
                                  GT => lh

