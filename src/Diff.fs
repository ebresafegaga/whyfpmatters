module Diff

type 'a listof = Nil | Cons of 'a Lazy * 'a listof Lazy

let (|Cons'|Nil'|) = function
    | Cons (Lazy a, Lazy rest) -> Cons' (a, rest)
    | Nil -> Nil'
  
let rec foldr f x = function 
    | Nil' -> x
    | Cons' (a, l) -> f a (foldr f x l)

let map f l = foldr (fun a b -> (Cons (lazy f a, lazy b))) Nil l

let rec repeat f a = Cons (lazy a, lazy repeat f (f a))

let easydiff f (x:float) h = (f (x+h) - f x)/h


let halve x = x/2.
let differentiate h0 f x = map (easydiff f x) (repeat halve h0)