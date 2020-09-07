module Newton

type 'a listof = Nil | Cons of 'a Lazy * 'a listof Lazy

let (-.) (a:float) b = a - b

let next n x = (x + float n/float x)/2.

let rec repeat' f a = seq { yield a; yield! repeat' f (f a) }

let rec repeat f a = Cons (lazy a, lazy repeat f (f a))

let rec within eps = function  
    Cons (Lazy a, Lazy (Cons (Lazy b, rest))) -> 
        if abs (a -. b) <= eps 
        then b
        else within eps (Cons (lazy b, rest))

let mysqrt a0 eps n = within eps (repeat (next n) a0)

let rec relative eps = function
    Cons (Lazy a, Lazy (Cons (Lazy b, rest))) -> 
        if abs (a/b -. 1.) <= eps 
        then b
        else relative eps (Cons (lazy b, rest))

let relativesqrt a0 eps n = relative eps (repeat (next n) a0) 