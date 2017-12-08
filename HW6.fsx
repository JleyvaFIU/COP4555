#load "skeleton.fsx"

//Import the parser
open Parser.Parse

//Import Skeleton
open Skeleton

(*
type term =
  | ID of string | NUM of int | BOOL of bool | SUCC | PRED | ISZERO
  | IF of term * term * term | APP of term * term
  | FUN of string * term | REC of string * term | ERROR of string
*)
//type typ = VARIABLE of string | INTEGER | BOOLEAN | ARROW of typ * typ

// Identity substitution.
//let I (t : typ) = t

let rec W (env, e) =
    printf "Expression: %A\n" e
    match e with
    | NUM n  -> 
        printf "Returning INT\n"
        (I, INTEGER)
    | BOOL b -> 
        printf "Returning BOOL\n"
        (I, BOOLEAN)
    | SUCC -> 
        printf "Returning INT -> INT\n"
        (I, ARROW(INTEGER, INTEGER))
    | PRED -> 
        printf "Returning INT -> INT\n"
        (I, ARROW(INTEGER, INTEGER))
    | ISZERO -> 
        printf "Returning INT -> BOOL\n"
        (I, ARROW(INTEGER, BOOLEAN))
    | FUN (x, b) -> 
        let a = newtypevar()
        let (s,t) = W (update env x a, b)
        //printf "Substitution: %A\n" (s a)
        printf "Unifying 4\n"
        let s2 = unify(s a, t)
        //printf "Unification: %A\n" s2
        (s2 << s, ARROW(s2 a,t))
    | REC (a, b) ->
        ///let var = newtypevar()
        ///let replacement = ARROW(var, var)
        ///printf "Replacement is of type: %A\n" replacement
        let (s, t) = W(env, b) //Find type of body of REC
        ///printf "Unifying 1\n"
        let thetype = ARROW(t, t)
        ///let s2 = unify(s replacement, thetype)
        ///printf "Unification is of type: %A t is of type %A\n" s2 t
        (s, s thetype)
    | APP (m1, m2) -> 
        let (s1, t1) = W(env, m1) //Find the type of m1
        let output = match (t1) with
          | ARROW(input, output) -> output
          | anything -> anything
        let (s2, t2) = W(s1 << env, m2) //Find the type of m2
        printf "Unify t1 %A with t2 %A\n" t1 t2
        let s3 = match (s2 t1) with
          | ARROW(x, y) -> 
            let a = newtypevar()
            printf "Unifying 2\n"
            unify(s2 t1, ARROW(t2, a)) //Unify the argument to the function
          | _ -> 
            printf "Unifying 5\n"
            unify(s2 t1, t2) //Unify two elements
        printf "s3 is %A\n" (s3 t1)
        (s3 << s2 << s1, s3 output) //Return the environment, with the modification to the type
    | IF (e1, e2, e3) ->
        let (s1, t1) = W (env, e1) //Find type of e1
        let s2 = unify (t1, BOOLEAN) //e1 must be a boolean
        let (s3, t2) = W (s2 << s1 << env, e2) //Find type of e2
        let (s4, t3) = W (s3 << s2 << s1 << env, e3) //Find type of e3
        printf "Unifying 3\n"
        let s5 = unify (s4 t2, t3) //Make sure e2 has the same type as e3
        (s5 << s4 << s3 << s2 << s1, s5 t3) //Return
    | ID x -> (I, VARIABLE x)
    | x -> failwith "Some case was incomplete";;


let input00 = parsestr "if false then 1 else 2";;
let input0 = parsestr "if iszero 0 then succ 1 else succ 2";;
let input11 = parsestr "fun x -> succ x";;
let input12 = parsestr "fun x -> fun y -> succ x";;
let input13 = parsestr "fun x -> succ ((fun y -> succ y) x)";;
let input131 = parsestr "a 1";;
let input14 = parsestr "rec a -> a 1";;
let input = parsestr "(rec d -> fun n -> if iszero n then 0 else succ (succ (d (pred n)))) 3";;
let input1= parsestr "let Ack = rec A ->  fun x -> fun y ->if iszero x then  succ y  else if iszero y then  A (pred x) 1  else  A (pred x) (A x (pred y))in  Ack 3 6";;
let input2 = parsestr "let plus = rec p -> fun x -> fun y -> if iszero x then y else p (pred x) (succ y) in  let times = rec t -> fun x -> fun y -> if iszero x then 0 else plus y (t (pred x) y)in  let factorial = rec f ->fun n -> if iszero n then 1 else times n (f (pred n))in factorial 6";;

let (s, t) = W(emptyenv, input00);;
let result = s t;;
typ2str result;;