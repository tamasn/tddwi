module Average

||| Calculates the average length of words in the string
||| @str The input string
export
average : (str : String) -> Double
average str = let strs : List String = words str
                  wordCount : Nat = length strs
                  charCount : Nat = sum (map length strs) in
                  cast charCount / cast wordCount
