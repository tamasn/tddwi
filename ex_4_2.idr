import Data.Vect

vectTake : (x : Fin n) -> Vect n a -> Vect (cast x) a 
vectTake FZ (x :: xs) = []
vectTake (FS k) (y :: ys) = y :: (vectTake k ys)


sumEntries : Num a => (pos : Integer) -> Vect (S n) a -> Vect (S n) a -> Maybe a
sumEntries {n} pos xs ys = case (integerToFin pos n) of
                            Nothing => Nothing
                            (Just x) => Just (index' x xs ys)
  where
    index' : Fin l -> Vect (S l) a -> Vect (S l) a -> a
    index' FZ (x :: xs) (y :: ys) = x + y
    index' (FS k) (y :: xs) (z :: ys) = index' k xs ys
