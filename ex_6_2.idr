import Data.Vect

Matrix : Nat -> Nat -> Type
Matrix x y = Vect x (Vect y Double)

testMatrix : Matrix 2 3
testMatrix = [[0, 0, 0], [0, 0, 0]]

data Format = Number Format
            | Dbl Format
            | Str Format
            | Chr Format
            | Lit String Format
            | End

PrintfType : Format -> Type
PrintfType (Number x) = (i : Int) -> PrintfType x
PrintfType (Dbl x) = (d : Double) -> PrintfType x
PrintfType (Str x) = (s: String) -> PrintfType x
PrintfType (Chr x) = (c: Char) -> PrintfType x
PrintfType (Lit x y) = PrintfType y
PrintfType End = String

printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number x) acc = \i => printfFmt x (acc ++ show i)
printfFmt (Dbl x) acc = \d => printfFmt x (acc ++ show d)
printfFmt (Str x) acc = \s => printfFmt x (acc ++ s)
printfFmt (Chr x) acc = \c => printfFmt x (acc ++ show c)
printfFmt (Lit x y) acc = printfFmt y (acc ++ x)
printfFmt End acc = acc

toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: xs) = Number (toFormat xs)
toFormat ('%' :: 's' :: xs) = Str (toFormat xs)
toFormat ('%' :: 'c' :: xs) = Chr (toFormat xs)
toFormat ('%' :: 'f' :: xs) = Dbl (toFormat xs)
toFormat ('%' :: xs) = Lit "%" (toFormat xs)
toFormat (x :: xs) = case toFormat xs of
                          (Lit x' y) => Lit (strCons x x') y
                          fmt => Lit (strCons x "") fmt

printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""

TupleVect : (n : Nat) -> Type -> Type
TupleVect Z x = ()
TupleVect (S k) x = (x, TupleVect k x)

test : TupleVect 4 Nat
test = (1,2,3,4,())
