module Main exposing (..)

esPrimo: Int -> Bool
esPrimo x =  esPrimoN 2 x
esPrimoN  contador n =
    if x == 2 
    then True 
    else if modBy   contador  x  == 0 
    then False 
    else if contador == x-1
    then True
    else esPrimoN (contador + 1) x

fibonacci:  number -> number1
fibonacci x = 
    if x == 0 
    then 0
    else if x == 1
    then 1 
    else if x > 1
    then fibonacci(x-1) + fibonacci(x-2)
    else 5

primos: Int -> List Int
primos x = 
    if x < 2
    then []
    else if esPrimo x == False 
    then primos (x - 1)
    else x :: primos (x - 1)

nPrimos: Int -> List Int
nPrimos x = conta (x , 2) 
conta (x,y) = 
    if x == 0 
    then []
    else if esPrimo y == False
    then conta (x,y + 1)
    else y :: conta (x-1, y+1)