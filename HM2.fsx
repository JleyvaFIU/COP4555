//#1 - Done. Build a cartesian product
let rec cartesian = function
  | ([], []) -> []
  | (_, []) -> []
  | ([], _) -> []
  | (a::[], head::tail) -> (a, head) :: (cartesian([a], tail))
  | (x::xs, y::ys) -> (x, y) :: (cartesian([x], ys)) @ (cartesian(xs, y::ys))

//#2 - Fail
let powerset input = 
    let level1_powerset element = List.map ( fun b -> [b]) element
    let output = [] :: (level1_powerset input) @ [input] //Prepend the empty set, append the initial set
    let level2_powerset (x::xs) = x :: [(remove_head xs)] //here we need to append the value to a list, otherwise it will fail
    let rec funct = function
        | [] -> []
        | (a::[]) -> []
        | (x::xs) -> level2_powerset (x::xs) :: (if (List.length xs) > 0 then (funct xs) else [])

//#3 - Done. Transpose a matrix of m x n
//Requires remove_first
let rec transpose = function
    | [] -> []
    | list -> (List.map (List.head) list) :: if (List.length (List.head list)) > 1 then (transpose (List.map (remove_first) list)) else [] //Just need to remove first element of each list on this recursive call

//Removes the first element from any list
let remove_first = function
    | [] -> []
    | x::xs -> xs

//Returns the head of a list
let remove_head xs = List.head xs

//#4 - Analysis
let rec sort = function
  | []         -> []
  | [x]        -> [x]
  | x1::x2::xs -> if x1 <= x2 then x1 :: sort (x2::xs)
                              else x2 :: sort (x1::xs)
//Make sure that each base case returns the correct answer: Covered. If you provide the empty list, the function returns the empty list. 
//If you provide a single value, the function returns a single value
//Make sure that each non-base case returns the correct answer, assuming that each of its recursive calls returns the correct answer: Covered.
//The function correctly sorts x1 and x2, and relies on sorting of the base cases to be done correctly
//Make sure that each recursive call is on a smaller input: Covered. Each call to sort either misses x1 or x2, making it work.

//Sample for sanity check
let rec concat = function
    | ([], []) -> []
    | (x::xs, y::ys) -> x :: y :: (concat(xs, ys))