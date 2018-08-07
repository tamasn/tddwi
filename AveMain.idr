module Main

import Average

showAverage : String -> String
showAverage str = let avg : Double = Average.average str
                      avgStr : String = show avg in
                      "The average word length is " ++ avgStr ++ "\n" 
                  
main : IO ()
main = repl "Enter a string: " showAverage
