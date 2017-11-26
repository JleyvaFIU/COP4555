//1. Define infix operators .+ and .* to do addition and multiplication of fractions.

// Function gcd (greatest common denominator) checks what the gcd is in a pair, which represents a fraction (a,b) //using recursion. If a single number is given it assumes b is 0. The function follows Euclids’s algorithm and was //provided by Professor Geoffrey Smith.

let rec gcd = function
  | (a,0) -> a
  | (a,b) -> gcd (b, a % b)
  
// Simplifies the pair, which represents a fraction (a,b) and returns it in the lowest terms.

let lowestF (a,b) =
    let gcdV=gcd (a,b)
    (a/gcdV,b/gcdV)

// Multiplies fractions together using two pairs (a,b) and (c,d) and returns the multiplication of the fraction in the lowest terms.
let (.*) (a,b) (c,d) =
        lowestF(a*c, b*d)

// Adds fractions together using two pairs (a,b) and (c,d) and returns the product of the fraction in the lowest //terms.
let (.+) (a,b) (c,d) =
    let sumD=b*d    
    let sumN=sumD/b*a + sumD/d*c
    lowestF(sumN,sumD)
  
 
//2. Revert a list of list

//Takes a list of lists l and reverses all of the sub-lists.

let revlists l = List.map (fun x -> List.rev x) l

//3. Write an F# function interleave(xs,ys) that interleaves two lists.

// Function interleave takes in two lists of equal size and interleaves the lists, alternating from the first
//item in the first list and adding the first item in the second list and so on.
// i.e List 1 = [1;2;3], List 2 = [4;5;6] Interleave Result = [1;4;2;5;3;6]   

let rec interleave = function 
    | ([],[]) -> []
    | (x::xs,[])  -> x::interleave(xs,[])
    | ([],y::ys)  -> y::interleave([],ys)
    | (x::xs, y::ys)  -> x::y::interleave(xs,ys)

//4. Write an F# function cut xs that cuts a list into two equal parts.

//To implement cut, first define an auxiliary function gencut(n, xs) inside of it 
//Gencut(n,xs) cuts xs into two pieces, where n gives the size of the first piece.
//We can find the size of half of the list by using List.length list/2 and use that in genlist to get the list split in half


let cut list=
    let gencut(n,list:'a list)=(list.[0..n-1],list.[n..List.length list-1])
    gencut(List.length list /2,list)


//5. Write an F# function shuffle xs that takes an even-length list, cuts it into two equal-sized pieces, and then //interleaves the pieces.

let shuffle (us) = cut us |> interleave

//6. Write an F# function countshuffles n that counts how many calls to shuffle on a deck of n distinct "cards" it //takes to put the deck back into its original order.

//Function countshuffles creates a list from 1…n and shuffles the list using the function shuffle from number 5.
// It shuffles the cards until they are back to original places and returns the number of shuffles it takes.
//The extra 1 is because it's shuffling once to start countaux
//What is the countshuffles for 52?
//countshuffles 52;;
//val it : int = 8
let countshuffles n =
    let xs = [1..n]
    let rec countaux = function
    | (xs, ys) -> if ( xs = ys ) then 0 else 1 + countaux(shuffle xs, ys)
    1 + countaux (shuffle xs, xs) 
    
    
    






