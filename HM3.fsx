let inner vector1 vector2 =
    let rec sub = function
        | ([], []) -> 0
        | (x::xs, y::ys) -> x*y + sub(xs, ys)
    sub(vector1, vector2)