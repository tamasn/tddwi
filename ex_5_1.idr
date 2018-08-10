printLonger : IO ()
printLonger = do
  putStrLn "First string: "
  s1 <- getLine
  putStrLn "Second string: "
  s2 <- getLine
  let l1 = length s1
  let l2 = length s2
  let ll = if l1 > l2 then l1 else l2
  putStrLn ("The longer string's length is " ++ (cast ll))

printLonger' : IO ()
printLonger' = 
  putStrLn "First string: " >>= \_ =>
    getLine >>= \s1 =>
      putStrLn "Second string: " >>= \_ =>
        getLine >>= \s2 =>
          let l1 = length s1
              l2 = length s2
              ll = if l1 > l2 then l1 else l2 in
                putStrLn ("The longer string's length is " ++ (cast ll))

