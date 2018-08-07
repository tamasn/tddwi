import Data.Vect

my_length : List a -> Nat
my_length [] = 0
my_length (x :: xs) = 1 + my_length xs


append : elem -> Vect n elem -> Vect (S n) elem
append x [] = [x]
append x (y :: ys) = y :: append x ys

my_reverse_vect : Vect n elem -> Vect n elem
my_reverse_vect [] = []
my_reverse_vect (x :: xs) = append x (my_reverse_vect xs)

my_reverse0 : List elem -> (List elem -> List elem) -> List elem
my_reverse0 [] f = f []
my_reverse0 (x :: xs) f = my_reverse0 xs (\ys => x :: f ys)

my_reverse : List elem -> List elem
my_reverse [] = []
my_reverse (x :: xs) = my_reverse0 xs (\ys => x :: ys)

my_map : (a -> b) -> List a -> List b
my_map f [] = []
my_map f (x :: xs) = f x :: my_map f xs

my_map_vect : (a -> b) -> Vect len a -> Vect len b
my_map_vect f [] = []
my_map_vect f (x :: xs) = f x :: my_map_vect f xs
