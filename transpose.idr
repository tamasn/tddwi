import Data.Vect

createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

transposeHelper : (x : Vect n elem) -> (xsTrans : Vect n (Vect len elem)) -> Vect n (Vect (S len) elem)
transposeHelper [] [] = []
transposeHelper (x :: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                             zipWith (::) x xsTrans

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: (addMatrix xs ys)



multMatrix : Num a => Vect n (Vect m a) -> Vect m (Vect p a) -> Vect n (Vect p a)
multMatrix xs ys = multiplyMatrix0 xs (transposeMat ys)
  where
    multiplyVector : Num a => Vect m a -> Vect m a -> a
    multiplyVector [] [] = 0
    multiplyVector (x :: xs) (y :: ys) = x * y + (multiplyVector xs ys)
    buildVector : Num a => Vect p (Vect m a) -> Vect m a -> Vect p a
    buildVector xs ys = map (multiplyVector ys) xs
    multiplyMatrix0 : Num a => Vect n (Vect m a) -> Vect p (Vect m a) -> Vect n (Vect p a)
    multiplyMatrix0 xs ys = map (buildVector ys) xs
