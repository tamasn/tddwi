import Data.Vect

allLengths : Vect len String -> Vect len Nat
allLengths [] = Nil
allLengths (x :: xs) = length x :: allLengths xs

allLengths2 : Vect len String -> Vect len Nat
allLengths2 [] = []
allLengths2 (x :: xs) = length x :: allLengths2 xs
