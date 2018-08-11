data Format = Number Format
            | Str Format
            | Lit String Format
            | End

PrintfType : Format -> Type
PrintfType (Number x) = (i : Int) -> PrintfType x
PrintfType (Str x) = (s: String) -> PrintfType x
PrintfType (Lit x y) = PrintfType y
PrintfType End = String

printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number x) acc = \i => printfFmt x (acc ++ show i)
printfFmt (Str x) acc = \s => printfFmt x (acc ++ s)
printfFmt (Lit x y) acc = printfFmt y (acc ++ x)
printfFmt End acc = acc

toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: xs) = Number (toFormat xs)
toFormat ('%' :: 's' :: xs) = Str (toFormat xs)
toFormat ('%' :: xs) = Lit "%" (toFormat xs)
toFormat (x :: xs) = case toFormat xs of
                          (Lit x' y) => Lit (strCons x x') y
                          fmt => Lit (strCons x "") fmt

printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""

