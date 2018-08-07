module Main

main : IO ()
main = putStrLn (cast 'x')
    
StringOrInt : Bool -> Type
StringOrInt x = case x of
    True => Int
    False => String

getStringOrInt : (x: Bool) -> StringOrInt x
getStringOrInt x = case x of
    True => 94
    False => "Ninety four"

valToString : (x : Bool) -> StringOrInt x -> String
valToString x = case x of
    True => ?trueval
    False => ?falseval

Shape : Type
rotate : Shape -> Shape

double : Num x => x -> x
double x = x + x

twice : (a -> a) -> a -> a
twice f x = f (f x)

turnAround : Shape -> Shape
turnAround x = twice rotate x

quadruple : Num x => x -> x
quadruple x = twice double x