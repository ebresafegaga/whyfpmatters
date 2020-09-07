module Diff

type 'a listof = Nil | Cons of 'a Lazy * 'a listof

let rec foldr f x = function 
    | Nil -> x
    | Cons (Lazy a, l) -> f a (foldr f x l)

let map f l = foldr (f >> (fun a b -> (Cons (a, b)))) Nil l

let rec repeat f a = Cons (lazy a, repeat f (f a))

let easydiff f (x:float) h = (f (x+h) - f x)/h