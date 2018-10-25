module Main exposing (..)

zipWith : (a -> b -> c) -> List a -> List b -> List c
zipWith l x y  = 
    case (x, y) of
    ([], _) -> []
    (_, []) -> []
    (b::xs, c::ys) -> l b c ::zipWith l xs ys

groupBy : (a -> Bool) -> List a -> (List a, List a) 
groupBy n list = (exito n list, fracaso n list)

exito s list = case list of 
    [] -> []
    (b::xs) -> if s b then exito s xs else b:: exito s xs

fracaso s list = case list of 
    [] -> []
    (l::lss) -> if s l then l:: fracaso s lss else fracaso s lss

bind : Maybe a1 -> (a1 -> Maybe a) -> Maybe a
bind ls f = case ls of 
    Nothing -> Nothing 
    Just a -> f a 