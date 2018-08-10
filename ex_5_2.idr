module Main

import System

readNumber : IO (Maybe Nat) 
readNumber = 
  do
    str <- getLine
    if all isDigit (unpack str)
       then pure (Just (cast str))
       else pure Nothing
       
random: IO Nat
random =
  do
    t <- time
    let tStr : String = cast t
    let lastTwoDigits : String = pack (reverse (take 2 (reverse (unpack tStr))))
    let num : Nat = if lastTwoDigits == "00"
                       then Z
                       else cast lastTwoDigits
    pure num

guess : (target : Nat) -> (guesses : Nat) -> IO ()
guess target guesses = 
  let guessStr : String = cast guesses
      prompt : String = (guessStr ++ ". guess: ") in
  do
    putStrLn prompt
    Just num <- readNumber | Nothing => do
      putStrLn "Invalid number"
      guess target guesses
    if target == num
      then putStrLn "You guessed the number"
      else
        let response = if num > target
                          then "too high"
                          else "too low" in
                            do
                              putStrLn response
                              guess target (guesses + 1)

main : IO ()
main = 
  do
    target <- random
    guess target 1

repl' : String -> (String -> String) -> IO ()
repl' prompt f = do
  putStr prompt
  input <- getLine
  putStr (f input)
  repl' prompt f

replWith' : a -> String -> (a -> String -> Maybe (String, a)) -> IO ()
replWith' s p f = do
  putStr p
  i <- getLine
  Just (o, s') <- pure (f s i) | Nothing => pure ()
  putStr o
  replWith' s' p f

