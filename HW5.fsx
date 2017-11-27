#load "parser.fsx"

//Import the parser
open Parser.Parse

//Sample use of the parser
let input = parsestr "(rec d -> fun n -> if iszero n then 0 else succ (succ (d (pred n)))) 3";;
let input2 = parsestr "let plus = rec p -> fun x -> fun y -> if iszero x then y else p (pred x) (succ y) in  let times = rec t -> fun x -> fun y -> if iszero x then 0 else plus y (t (pred x) y)in  let factorial = rec f ->fun n -> if iszero n then 1 else times n (f (pred n))in factorial 6";;

let input1= parsestr "let Ack = rec A ->  fun x -> fun y ->if iszero x then  succ y  else if iszero y then  A (pred x) 1  else  A (pred x) (A x (pred y))in  Ack 3 6";;
(** 
 * Substitutes a string/ID x in a term e with a term t
 *
 * @param term e - The term that's going to be modified
 * @param ID x - The target or needle
 * @param term t - The term that's going to be used to replace x where x is found
 * @return term - The term e where x was replaced by t
 *)
let rec subst e x t = 
    match e with
    | ERROR x -> ERROR x //If we find an error, return it
    | APP(f1,f2) -> APP(subst f1 x t,subst f2 x t)  //If we are substituting an APP, carry the search down
    | FUN(y,f1)  -> if x=y then FUN(y,f1) else FUN(y,subst f1 x t)
    | REC(y,f1)  -> if x=y then REC(y,f1) else REC(y,subst f1 x t)
    | IF(f1,f2,f3) -> IF(subst f1 x t, subst f2 x t, subst f3 x t)
    | NUM n -> NUM n
    | ID y  -> if x=y then t else ID y ///If we are substituting an ID and it matches x, replace it, else return the original
    | all -> all // if expression not in any case return it as it is. 
//Implementing the interpreter
(**
let rec interp = function
    | ERROR x -> ERROR x //If there is an error, return the error
    | NUM n when n > 0 -> NUM n //If it is a number (greater than 0, as specified), return the number
    | BOOL b -> BOOL b //If it is a boolean, return the boolean
    | SUCC -> SUCC //If it is the SUCC term, return it. We don't evaluate here
    | PRED -> PRED //If it is the PRED term, return it. We don't evaluate here
    | ISZERO -> ISZERO //If it is the ISZERO term, return it. We don't evaluate here
    | ID i -> ID i
    | REC(x,y) -> REC(x,y)
    | FUN (x, y) -> FUN(x, y) //This is the identity function. No evaluation here.
    | IF (BOOL b, e1, e2) -> if b then interp e1 else interp e2 //Evaluate the IF function. b must be a BOOL term, otherwise, fall down
    | IF (APP (f1, f2), e1, e2) -> interp (IF((interp (APP(f1, f2))), e1, e2)) //Evaluate the IF function. b can be any APP term
    | APP (SUCC, NUM f2) -> NUM (f2 + 1) //Evaluate the SUCC term
    | APP (PRED, NUM f2) -> NUM (f2 - 1) //Evaluate the PRED term
    | APP (ISZERO, NUM f2) -> BOOL (f2 = 0) //Evaluate the ISZERO term
    | APP (FUN(x, e), t) -> interp (subst e x t)
    | APP (REC(x,e),t) -> interp (APP (subst e x (REC (x, e)), t)) // in recursion with make the substitition passing the recursive function (Rule 11)
    | APP (f1, f2) -> interp (APP (f1, interp f2)) //Evaluate any APP term where the second term is not a NUM    
    | NUM n -> NUM n
    |  all -> all

*)


//From Skeleton 
let rec interp = function
| APP (e1, e2) ->
    match (interp e1, interp e2) with
    | (ERROR s, _)  -> ERROR s        // ERRORs are propagated
    | (_, ERROR s)  -> ERROR s
    | (SUCC, NUM n) -> NUM (n+1)      // Rule (6)
    | (SUCC, v)     -> ERROR (sprintf "'succ' needs int argument, not '%A'" v)
    | (PRED, NUM n) -> NUM (n-1)      //Evaluate the PRED term
    | (PRED, v)     -> ERROR (sprintf "'pred' needs int argument, not '%A'" v)
    | (ISZERO, NUM f2) -> BOOL (f2 = 0) //Evaluate the ISZERO term
    | (ISZERO, v)      -> ERROR (sprintf "'ISZERO' needs INT argument, not '%A'" v)
    | (FUN(x, e), t) -> interp (subst e x t)
    | (REC(x,e),t) ->   interp (APP (subst e x (REC (x, e)), t)) // in recursion with make the substitition passing the recursive function (Rule 11)
| IF (f1,f2,f3) ->
    match (interp f1, f2, f3) with
    | (ERROR s, _, _)       -> ERROR s
    | (_, ERROR s, _)       -> ERROR s
    | (_, _, ERROR s)       -> ERROR s
    | (BOOL true, e, _)     -> interp e
    | (BOOL false, _, e)    -> interp e
    | (b, _, _) -> ERROR "After evaluated conditionmust be boolean"
 | NUM n  -> NUM n 
 | BOOL b -> BOOL b //If it is a boolean, return the boolean
 | SUCC -> SUCC //If it is the SUCC term, return it. We don't evaluate here
 | PRED -> PRED //If it is the PRED term, return it. We don't evaluate here
 | ISZERO -> ISZERO //If it is the ISZERO term, return it. We don't evaluate here
 | ID i -> ID i
 | REC(x,y) -> REC(x,y)
 | FUN (x, y) -> FUN(x, y) //This is the identity function. No evaluation here.
 | all->all;;

 interp input;;
 interp input1;;
 interp input2;;