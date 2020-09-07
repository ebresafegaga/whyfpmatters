module Lists

type 'a listof = Nil | Cons of 'a * 'a listof

let rec sum = function
    | Nil -> 0
    | Cons (x, xs) -> x + sum xs

let rec foldr f x = function 
    | Nil -> x 
    | Cons (a, l) -> f a (foldr f x l)

let sum' = foldr (+) 0

let product = foldr (*) 1

let anytrue = foldr (||) false

let alltrue = foldr (&&) true

let l = Cons (1, Cons (2, Cons (3, Nil)))

let append a b = foldr (fun a s -> Cons (a, s)) b a 

let count a n = n + 1
let length l = foldr count 0 l

let doubleandcons n list = Cons (2*n, list)
let doublecall = foldr doubleandcons Nil 

let fandcons f = f >> (fun a b -> Cons (a, b)) 

let map f l = foldr (f >> (fun a b -> (Cons (a, b)))) Nil l

let summatrix = map sum >> sum

let m = 
    Cons (
        Cons (1, Cons (2, Cons (3, Nil))), 
            Cons (Cons (3, Cons (4, Cons (5, Nil))), 
                Cons (Cons (6, Cons (5, Cons (7, Nil))),
                    Nil)))