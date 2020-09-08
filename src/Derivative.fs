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

let (-.) (a:float) b = a - b
let rec within eps = function  
    | Cons' (a, Cons' (b, rest)) -> 
        if abs (a -. b) <= eps 
        then b
        else within eps (Cons (lazy b, lazy rest))

let easydiff f (x:float) h = (f (x+h) - f x)/h


let halve x = x/2.
let differentiate h0 f x = map (easydiff f x) (repeat halve h0)

let rec elimerror n = function
    | Cons' (a, Cons' (b, rest)) -> 
        Cons (
            lazy ((b * (2.**n) - a)/(2.**(n-1.))), 
            lazy elimerror n (Cons (lazy b, lazy rest)))

let log2 = System.Math.Log2

let order = function 
    | Cons' (a, Cons' (b, Cons' (c, rest))) ->
        round (log2 (a-c)/(b-c) - 1.)

let improve s = elimerror (order s) s

let dx h0 f x eps = within eps (improve (differentiate h0 f x)) 

let seceond (Cons' (_, (Cons' (b, _)))) = b  
let super s = map seceond (repeat improve s)

let dx' h0 f x eps = within eps (super (differentiate h0 f x)) 