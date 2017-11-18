#load "parser.fsx"

//Import the parser
open Parser.Parse

//Sample use of the parser
let input = parsestr "iszero (succ 7)";;
let input1 =  parsestr "if iszero 0 then 1 else 2";;
let input2 = parsestr "if iszero 2 then succ 5 else succ 10";;
let input3 = parsestr "if iszero 0 then succ 5 else succ 10";;
let input4 = parsestr "if iszero 1 then succ 5 else pred 10";;

//Implementing the interpreter
let rec interp = function
    | ERROR x -> ERROR x //If there is an error, return the error
    | NUM n when n > 0 -> NUM n //If it is a number (greater than 0, as specified), return the number
    | BOOL b -> BOOL b //If it is a boolean, return the boolean
    | SUCC -> SUCC //If it is the SUCC term, return it. We don't evaluate here
    | PRED -> PRED //If it is the PRED term, return it. We don't evaluate here
    | ISZERO -> ISZERO //If it is the ISZERO term, return it. We don't evaluate here
    | IF (BOOL b, e1, e2) -> if b then interp e1 else interp e2 //Evaluate the IF function. b must be a BOOL term, otherwise, fall down
    | IF (APP (f1, f2), e1, e2) -> interp (IF((interp (APP(f1, f2))), e1, e2)) //Evaluate the IF function. b can be any APP term
    | APP (SUCC, NUM f2) -> NUM (f2 + 1) //Evaluate the SUCC term
    | APP (PRED, NUM f2) -> NUM (f2 - 1) //Evaluate the PRED term
    | APP (ISZERO, NUM f2) -> BOOL (f2 = 0) //Evaluate the ISZERO term
    | APP (f1, f2) -> interp (APP (f1, interp f2)) //Evaluate any APP term where the second term is not a NUM
    | _ -> failwith "Not valid or not implemented"