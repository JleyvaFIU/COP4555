(** 
 * 1- 
 * Finds the product of two vectors
 * Status - Done
 *
 * @param int list list - vector1 
 * @param int list list - vector2
 * @return int
 *)
let inner vector1 vector2 =
    let rec sub = function
        | ([], []) -> 0
        | (x::xs, y::ys) -> x*y + sub(xs, ys)
    sub(vector1, vector2)

(**
 * Helper for 2
 * Transpose. Transposes a list. See Excel terminology if confused.
 *
 * @param 'a list list - The list to transpose
 * @return - 'a list list - The list, transposed
 *)
let rec transpose = function
    | [] -> []
    | list -> (List.map (List.head) list) :: if (List.length (List.head list)) > 1 then (transpose (List.map (remove_first) list)) else [] //Just need to remove first element of each list on this recursive call

(**
 * Helper for 2 - Helper for transpose
 * Removes the first element from any list
 *
 * @param list - The list that needs the first element removed
 * @return - The tail of the list
 *)
let remove_first = function
    | [] -> []
    | x::xs -> xs

(**
 * 2-
 * Weird matrix multiplication.
 * Problem: Given an m-by-n matrix A and an n-by-p matrix B, the product of A and B is an 
 * m-by-p matrix whose entry in position (i,j) is the inner product of row i of A with column j of B. For example,
 *                / 0 1 \
 *  / 1 2 3 \  *  | 3 2 |  =  /  9 11 \
 *  \ 4 5 6 /     \ 1 2 /     \ 21 26 /
 *
 * Status: Done
 * 
 * @param int list list * int list list - the two lists to multiply
 * @return int list list - The matrix with the multiplication results
 *)
let multiply (matrix1, matrix2) = 
  let matrix2_transposed = transpose matrix2

  let rec inner_multiply = function
  | ([], []) -> []
  | (_, []) -> []
  | ([], _) -> []
  | (x::xs, y::ys) -> (List.map (fun z -> inner x z) (y::ys)) :: inner_multiply (xs, y::ys)

  inner_multiply (matrix1, matrix2_transposed)

(**
 * 5-
 * List.map on infinite streams
 * Problem: Show how to define map f s on streams; this should give the stream formed by applying function f to each element of stream s.
 * Status: Done
 *
 * @param f - a function to apply on the stream
 * @param Cons(x, xsf) - The stream to apply the function on
 * @return Cons(y, ysf) - The stream Cons(x, xsf) with the function f applied to it
 *)
type 'a stream = Cons of 'a * (unit -> 'a stream)


let rec map f (Cons(x, xsf)) = 
    Cons(f x, fun() -> map f (xsf()))


