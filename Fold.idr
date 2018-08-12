totalLen : List String -> Nat
totalLen xs = foldr (\str, len => length str + len) 0 xs

data Tree a = Empty | Node (Tree a) a (Tree a)

Foldable Tree where
  foldr func init Empty = init
  foldr func init (Node left a right) =
    let leftFold = foldr func init left
        rightFold = foldr func leftFold right in
        func a rightFold
