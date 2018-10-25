module Main exposing (..)

type Natural = Suc Natural | Cero

enteroNatural: Int -> Natural
enteroNatural i = if i == 0 
    then Cero 
    else Suc(enteroNatural (i-1))

natural: Natural -> Int
natural n = case n of 
    Cero -> 0
    Suc i -> 1 + natural i

sum n1 n2 = case (n1, n2) of
    (Cero, n2_) -> n2_
    (n1_, Cero) -> n1_
    (Suc n1_, n2_) -> Suc (sum n1_ n2_)

mult: Natural -> Natural -> Natural
mult n1 n2 = case (n1,n2) of 
    (Cero, n2_) -> Cero
    (n1_, Cero) -> Cero
    (Suc n1_, n2_) -> sum n2_ (mult n1_ n2_)

resta: Natural -> Natural -> Natural
resta n1 n2 = case (n1, n2) of 
    (Cero, n2_) -> Cero
    (n1_, Cero) -> n1_
    (Suc n1_, Suc n2_) -> (resta n1_ n2_)

divn: Natural -> Natural -> Natural -> Natural
divn n1 n2 x = case (n1, n2, x) of
    (Cero , n2_ , x_) -> x_
    (n1_, Cero , x_) -> Cero    
    (n1_,  n2_ , x_) -> divn (resta n1_ n2_) n2_ (Suc x_)

division: Natural -> Natural -> (Natural, Natural)
division n1_ n2_ =
    if mult (divn n1_ n2_ Cero) n2_ == n1_ 
    then (divn n1_ n2_ Cero, Cero)  
    else (resta (divn n1_ n2_ Cero) (Suc Cero), resta (n1_ )(mult (resta(divn n1_ n2_ Cero)(Suc Cero)) n2_ ))


type GExpresion a = Valor a 
    | Suma (GExpresion a) (GExpresion a)
    | Mult (GExpresion a) (GExpresion a)

type alias Expresion = GExpresion Int

type Estado = Final Int 
    | Parce (List Char)
