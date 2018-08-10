import Data.Vect

readVectLen : (len : Nat) -> IO (Vect len String)
readVectLen Z = pure [] 
readVectLen (S k) = do x <- getLine
                       xs <- readVectLen k
                       pure (x :: xs)

data VectUnknown : Type -> Type where
  MkVect : (len : Nat) -> Vect len a -> VectUnknown a

readVect' : IO (VectUnknown String)
readVect' = do x <- getLine
               if (x == "")
                  then pure (MkVect _ [])
                  else do MkVect _ xs <- readVect'
                          pure (MkVect _ (x :: xs))

printVect : Show a => VectUnknown a -> IO ()
printVect (MkVect len xs)
  = putStrLn (show xs ++ " (length " ++ show len ++ ")")

readVect : IO (n ** Vect n String)
readVect = do x <- getLine
              if (x == "")
                 then pure (_ ** [])
                 else do (_ ** xs) <- readVect
                         pure (_ ** x :: xs)
                         
zipInputs : IO ()
zipInputs = do putStrLn "Enter first vector (blank line to end):"
               (len1 ** vec1) <- readVect
               putStrLn "Enter second vector (blank line to end):"
               (len2 ** vec2) <- readVect
               case exactLength len1 vec2 of
                    Nothing => putStrLn "Vectors are different lengths"
                    Just vec2' => printLn (zip vec1 vec2')

