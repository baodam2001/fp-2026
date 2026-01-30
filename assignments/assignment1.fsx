(* Exercise 1.1 *)
let sqr x = x * x

(* Exercise 1.2 *)
let pow x n = System.Math.Pow(x, n)

(* Exercise 1.3 - Solve HR, exercise 1.1 *)
let g n = n + 4

(* Exericse 1.4 - Solve HR, exercise 1.2 *)
let h (x, y) = System.Math.Sqrt(x*x + y*y)

(* Exercise 1.5 - Solve HR, exercise 1.4 *)
let rec f = function
| 0 -> 0
| n -> n + f(n-1)
// _arg1 is unnamed argument, because no argument is shown in the declaration

(* Exercise 1.6 - Solve HR, exercise 1.5 *)
let rec fib = function
| 0 -> 0
| 1 -> 1
| n -> f(n-1) + f(n-2)
(*
    evaluation for F4:
    fib 4
    -> fib 3 + fib 2
    -> fib 2 + fib 1 + fib 1 + fib 0
    -> fib 1 + fib 0 + 1 + 1 + 0
    -> 1 + 0 + 1 + 1 + 0
    -> 3
*) 

(* Exercise 1.7 Solve HR, exercise 1.6 *)
let rec sum = function
| (m, 0) -> m
| (m , n) -> (m + n) + sum(m, n-1)
(*
    Recursion formula:
    sum(m, 0) = m
    sum(m, n) = (m+n) + sum(m, (n-1))
*)

(* Exercise 1.8 Solve HR, exercise 1.7 *)
(*
    (System.Math.PI, fact -1) of type float * int
    fact(fact 4) of type int
    power(System.Math.PI, fact 2) of type float
    (power, fact) of type float * int
*)

(* Exercise 1.9 Solve HR, exercise 1.8 *)
(* 
    Environment:
    a -> 5
    a -> a+1
    b -> [b -> b+1] + 5

    Evaluations:
    f 3
    -> 3 + 1
    -> 4

    g 3
    -> f 3 + a
    -> 3 + 1 + 5
    -> 9
*)

