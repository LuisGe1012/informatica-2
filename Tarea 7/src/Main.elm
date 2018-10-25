module Main exposing (..)

sum: Int -> Int -> Int 
sum a b = a + b 

mult: Int -> Int -> Int 
mult a b = a * b 

type Expresion = Sum Expresion Expresion | Mult Expresion Expresion  | Valor Int 
    

reducir : (Int -> Int -> Int , Int -> Int -> Int) -> Expresion -> Int
reducir (iSum, iMult) oper =
    case oper of
        Sum op1 op2 -> iSum (reducir (iSum, iMult) op1) (reducir (iSum, iMult) op2)
        Mult op1 op2 -> iMult (reducir (iSum, iMult) op1) (reducir (iSum, iMult) op2)
        Valor n -> n
