module Tree 


type Tree<'a> = Node of 'a * Tree<'a> | Sub of Tree<'a> list


let a = 
    Node (1, 
            Sub [ Node (2, Sub [])
                  Sub [ Node (3, Sub [ Node (4, Sub [])])]
                ])

let rec foldtree f g a t = 
    match t with
    | Node (label, subtrees) ->
        f label (foldtree f g a subtrees)
    | Sub (x :: xs) ->
        g (foldtree f g a x) (foldtree f g a (Sub xs))
    | Sub [] -> a

let result = foldtree (+) (+) 0 a

(* Don't try this at home *)
let Node a as' = Node (a, as')
let (++) x xs =
    match xs with
    | Sub ts -> Sub (x :: ts)
    | Node _ -> failwith "you just can't"
let empty = Sub []

let maptree f = foldtree (Node << f) (++) empty 

let result' = maptree ((+) 1) a