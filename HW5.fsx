#load "parser.fsx"

open Parser.Parse

let input = parsestr "iszero (succ 7)";;

let rec interp = function
    | ERROR x -> ERROR x
    | NUM n when n > 0 -> NUM n
    | BOOL b -> BOOL b
    | SUCC -> SUCC
    | PRED -> PRED
    | ISZERO -> ISZERO
    | IF (BOOL b, e1, e2) -> if b then e1 else e2
    | APP (SUCC, NUM f2) -> NUM (f2 + 1)
    | APP (PRED, NUM f2) -> NUM (f2 - 1)
    | APP (ISZERO, NUM f2) -> BOOL (f2 = 0)
    | APP (f1, f2) -> interp (APP (f1, interp f2))
    | _ -> failwith "Not valid or not implemented"