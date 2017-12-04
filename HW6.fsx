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
    | NUM n  -> (I, INTEGER)
    | BOOL b -> (I, BOOLEAN)
    | SUCC -> (I, ARROW(INTEGER, INTEGER))
    | PRED -> (I, ARROW(INTEGER, INTEGER))
    | ISZERO -> (I, ARROW(INTEGER, BOOLEAN))
    | APP (m1, m2) -> 
        let (s1, t1) = W(env, m1) //Find the type of m1
        let (s2, t2) = W(s1 << env, m2) //Find the type of m2
        let s3 = unify(s2 t1, t2) //Unify the argument to the function
        (s3 << s2 << s1, s3 t1) //Return the environment, with the modification to the type
    | IF (e1, e2, e3) ->
        let (s1, t1) = W (env, e1) //Find type of e1
        let s2 = unify (t1, BOOLEAN) //e1 must be a boolean
        let (s3, t2) = W (s2 << s1 << env, e2) //Find type of e2
        let (s4, t3) = W (s3 << s2 << s1 << env, e3) //Find type of e3
        let s5 = unify (s4 t2, t3) //Make sure e2 has the same type as e3
        (s5 << s4 << s3 << s2 << s1, s5 t3) //Return
    | x -> failwith "Some case was incomplete";;


let input00 = parsestr "if false then 1 else 2";;
let input0 = parsestr "if iszero 0 then succ 1 else succ 2";;
let input = parsestr "(rec d -> fun n -> if iszero n then 0 else succ (succ (d (pred n)))) 3";;
let input1= parsestr "let Ack = rec A ->  fun x -> fun y ->if iszero x then  succ y  else if iszero y then  A (pred x) 1  else  A (pred x) (A x (pred y))in  Ack 3 6";;
let input2 = parsestr "let plus = rec p -> fun x -> fun y -> if iszero x then y else p (pred x) (succ y) in  let times = rec t -> fun x -> fun y -> if iszero x then 0 else plus y (t (pred x) y)in  let factorial = rec f ->fun n -> if iszero n then 1 else times n (f (pred n))in factorial 6";;

W(emptyenv, input00);;