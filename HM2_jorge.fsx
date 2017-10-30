(*
2-----An F# list can be thought of as representing a set, where the order of the elements in the list is irrelevant.
Write an F# function powerset such that powerset set returns the set of all subsets of set. For example,
  > powerset [1;2;3];;
  val it : int list list
  = [[]; [3]; [2]; [2; 3]; [1]; [1; 3]; [1; 2]; [1; 2; 3]]
*)
//Using List.collect(fun x->[x;1::x]) [[2;3]];;
/// A variant of List.map. (Actually built-in as List.collect.)
let rec appmap f = function
| []    -> []
| x::xs -> f x @ appmap f xs

let rec powerset = 
  function
  | [] -> [[]]
  | y::ys -> appmap (fun x -> [x; y::x]) (powerset ys)





(*
Recall that an F# function that takes two arguments can be coded 
in either uncurried form (in which case it takes a pair as its input) or 
curried form (in which case it takes the first argument and returns a function that takes the second argument).
In fact it is easy to convert from one form to the other in F#. To this end, 
define an F# function curry f that converts an uncurried function to a curried function,
and an F# function uncurry f that does the opposite conversion. For example,
  > (+);;
  val it : (int -> int -> int) = <fun:it@13-7>
  > let plus = uncurry (+);;
  val plus : (int * int -> int)
  > plus (2,3);;
  val it : int = 5
  > let cplus = curry plus;;
  val cplus : (int -> int -> int)
  > let plus3 = cplus 3;;
  val plus3 : (int -> int)
  > plus3 10;;
  val it : int = 13


*)
(*
> (+);;
val it : (int -> int -> int) = <fun:it@3-9>

> let plus= uncurry it;;
val plus : (int * int -> int)

> plus(4,5);;
val it : int = 9
*)
let uncurry cFunc (a,b)= cFunc a b   


(*
> let cplus = curry plus;;
val cplus : (int -> int -> int)

> cplus 3;;
val it : (int -> int) = <fun:Invoke@3253>

> it 6;;
val it : int = 9

> 
*)
let curry uFunc a b = uFunc (a,b)





  
/// Insert a value into a list in all possible ways.
let rec insert x = function
| []    -> [[x]]
| y::ys -> (x::y::ys) :: List.map (fun zs -> y::zs) (insert x ys)

/// Find all permutations of a list of distinct elements.
let rec permute = function
| []    -> [[]]
| x::xs -> appmap (insert x) (permute xs)