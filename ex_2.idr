palindrome : Nat -> String -> Bool
palindrome minLength str = let wLength = length str
                               lCaseStr = toLower str
                               rev = reverse lCaseStr in
                               wLength >= minLength && rev == lCaseStr

counts : String -> (Nat, Nat)
counts str = (length (words str), length str)

top_ten : Ord a => List a -> List a
top_ten xs = take 10 (reverse (sort xs))

over_length : Nat -> List String -> Nat
over_length n xs = length (filter (\x => n < length x) xs)
