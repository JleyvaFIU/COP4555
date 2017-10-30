let inner vector1 vector2 =
    let rec sub = function
        | ([], []) -> 0
        | (x::xs, y::ys) -> x*y + sub(xs, ys)
    sub(vector1, vector2)

let rec transpose = function
    | [] -> []
    | list -> (List.map (List.head) list) :: if (List.length (List.head list)) > 1 then (transpose (List.map (remove_first) list)) else [] //Just need to remove first element of each list on this recursive call

//Given an m-by-n matrix A and an n-by-p matrix B, the product of A and B is an m-by-p matrix whose entry in position (i,j) is the inner product of row i of A with column j of B. For example,
 //               / 0 1 \
 // / 1 2 3 \  *  | 3 2 |  =  /  9 11 \
 // \ 4 5 6 /     \ 1 2 /     \ 21 26 /

//Work in progress
let rec multiply = function
| ([], []) -> 0
//| (x::[], y::[]) -> 
| (x::xs, y::ys) -> inner (xs) (ys)

multiply ( [ [1;2;3]; [4;5;6] ], [ [0;1]; [3;2]; [1;2] ] );;

//5 - Show how to define map f s on streams; this should give the stream formed by applying function f to each element of stream s.
type 'a stream = Cons of 'a * (unit -> 'a stream)

//This should work
let rec map f (Cons(x, xsf)) = 
    Cons(f x, fun() -> map f (xsf()))