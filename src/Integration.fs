module Integration

open Derivative

let rec zip2 (Cons' (a, s)) (Cons' (b, t)) = 
    Cons (lazy (a, b), lazy zip2 s t)
let addpair (a, b) = a + b + 0.

let easyintegrate f a b = (f a + f b) * (b - a) / 2.

let rec integrate f a b = 
    let mid = (a+b)/2.
    Cons (lazy easyintegrate f a b, 
          lazy (map addpair (zip2 (integrate f a mid)
                            (integrate f mid b))))

let rec integ f a b fa fb = 
    let m = (a+b)/2.
    let fm = f m
    Cons (lazy ((fa+fb)*(b-a)/2.), 
         lazy map addpair (zip2 (integ f a m fa fm)
                                (integ f m b fm fb)))
let integrate' f a b = integ f a b (f a) (f b)

let f' f a b eps = within eps (integrate' f a b)
// let f'' f a b eps = relative eps (integrate' f a b)

let g = super (integrate' sin 0. 4.)