(* Exercise 3.1 Solve HR, exercise 2.10 *)
let test(c,e) = if c then e else 0;;

(* 
part 1:
The type of test: 
bool * int -> int   // type reference
*)

(*
part 2:
The evaluation of test(false,fact(-1)) likely causes stack overflow error, 
because when c is false, the first branch is executed,
fact -1 is executed forever (it never reaches the base case)
*)

(*
part 3:
Compare test(false,fact(-1)) 
with the result of evaluating: if false then fact -1 else 0

for the if-else expression:
when the boolean value is false,
the result is fact -1, which leads to stack overflow
so the two results are the same
*)

(* Exercise 3.2 Solve HR, exercise 2.13 *)
(*
curry   : (’a * ’b -> ’c) -> ’a -> ’b -> ’c
curry f is the function g where g x is the function h where h y = f(x,y).
uncurry : (’a -> ’b -> ’c) -> ’a * ’b -> ’c
uncurry g is the function f where f(x,y) is the value h y for the function h = g x.

Declaration of curry:
let curry f x y = f(x, y)
the function curry has 3 arguments: f, x, y
f has type 'a * 'b -> 'c
x has type 'a
y has type 'b
(x, y) has type 'a * 'b
f(x, y) has type 'c
curry can be written as:
let curry =
    fun f ->
        fun x ->
            fun y ->
                f(x, y)

Declaration of uncurry:
let uncurry g = fun(x, y) -> g x y
the function uncurry has 1 argument: g
g has type ’a -> ’b -> ’c
fun(x, y) -> g x y has type ’a * ’b -> ’c
*)

(* Exercise 3.3 *)
let rec downTo n = 
    if n = 1 then [1]
    else n :: downTo (n-1)

let rec downTo2 n = 
    match n with
    | 1 -> [1]
    | _ -> n :: downTo2 (n-1)

(* Exercise 3.4 *)
let rec removeOddIdx = function
| [] -> []
| [x] -> [x]
| x1: int :: x2 :: xs -> x1 :: removeOddIdx xs          // type reference

removeOddIdx [1;3;4;5]

(* Exercise 3.5 *)
let rec combinePair = function
| [] -> []
| [x] -> []
| x1: int :: x2 :: xs -> (x1, x2) :: combinePair xs     // type reference

combinePair [1; 2; 3; 4; 5]

(* Exercise 3.6 Solve HR, exercise 3.2 *)
(* The former British currency had 12 pence to a shilling and 20 shillings to a pound. 
Declare functions to add and subtract two amounts, 
represented by triples (pounds, shillings, pence) of integers, 
and declare the functions when a representation by records is used. 
Declare the functions in infix notation with proper precedences, 
and use patterns to obtain readable declarations *)

// declare a record, bc means British currency
type bc = {pounds: int; shilling: int; pence: int}

let (.+.) (amount1: bc) (amount2: bc) =  
    let x = amount1.pounds * 20 * 12 + amount1.shilling * 12 + amount1.pence
    let y = amount2.pounds * 20 * 12 + amount2.shilling * 12 + amount2.pence
    x + y

let (.-.) (amount1: bc) (amount2: bc) =
    let x = amount1.pounds * 20 * 12 + amount1.shilling * 12 + amount1.pence
    let y = amount2.pounds * 20 * 12 + amount2.shilling * 12 + amount2.pence
    x - y

(* Exercise 3.7 Solve HR, exercise 3.3 *)
(* part 1 *)
// the parenttheses define symbolic/infix operator
let (.+.) (a, b) (c, d) = (a + c, b + d)
let (.*.) (a, b) (c, d) = (a*c - b*d, b*c + a*d)

(* part 2 *)
let (.-.) (a, b) (c, d) = 
    let (~-.) (c, d) = (-c, -d)
    (a, b) .+. -. (c, d)


// F# does not allow to define a prefix operator like this (~*.)
// so inv is used 
let (./.) (a, b) (c, d) =
    let inv (c, d) = 
        if c = 0 && d = 0 then 
            failwith "division by zero"
        else
            (c/(c*c + d*d), -d/(c*c + d*d))
    (a, b) .*. inv (c, d)

// (* part 3 *)
let (./.) (a, b) (c, d) =
    let denom = c*c + d*d
    let inv (c, d) = 
        if c = 0 && d = 0 then 
            failwith "division by zero"
        else
            (c/denom, -d/denom)
    (a, b) .*. inv (c, d)



(* Exercise 3.8 Solve HR, exercise 4.4 *)
let rec altsum = function
| [] -> 0
| [x] -> x
| x0::x1::xs -> x0 - x1 + altsum xs;;

// two clauses version
let rec altsum2 = function
| [] -> 0
| x0::xs -> x0 - altsum2 xs

altsum2 [2;-1;3]

(*
evaluation:
altsum2 [2;-1;3]
-> 2 - altsum2 [-1;3]
-> 2 - (-1 - altsum2[3])
-> 2 - (-1 - (3 - altsum2[0]))
-> 2 - (-1 - (3 - 0))
-> 2 -(-1 -3)
-> 2 -(-4)
-> 6

if the list has an two elements, altsum2 works exactly like altsum, the result is x0 - x1
if the list has more than two elements, for example, with four elements, the result is x0 - (x1 - (x2 - x3) 
we have x0 - (x1 - (x2 - x3) 
        = x0 - (x1 - x2 + x3)
        = x0 - x1 + x2 - x3
        = (x0 - x1) + (x2 - x3), similar to the result of altsum

The subtraction sign (-) do the magic here :) 
I think altsum is the one of a few functions can work correctly with both three and two clauses version like this
*)

