(* Exercise 2.1 *)
// make use of type inference
let dup x = x + x: string

(* Exercise 2.2 *)
// using if-else statement:

// let rec dupn s n =
//     if n = 0 then
//         ""
//     else 
//         s + dupn s (n-1)

// using pattern matching:
// cannot use "function" keyword because it's for function with only one argument
let rec dupn s n = 
    match n with
    | 0 -> ""
    | n -> s + dupn s (n-1) 

(* Exercise 2.3 *)
// can be done without the two let-expressions (less code)
let timediff (hh1, mm1) (hh2, mm2) = 
    let t1 = hh1*60 + mm1
    let t2 = hh2*60 + mm2
    t2 - t1

(* Exercise 2.4 *)
let minutes (hh, mm) = timediff(0, 0) (hh, mm)

(* Exercise 2.5 Solve HR, exercise 2.2 *)
// this exercise is similar to Exercise 2.2 above, except that the function takes a tuple as an argument, not two separate arguments
// here "function" can be used 'cause there is only one argument, unlike Exercise 2.2
let rec pow = function
| (_, 0) -> ""
| (s, n) -> s + pow(s, n-1)

(* Exercise 2.6 Solve HR, exercise 2.8 *)
let rec bin (n, k) = 
    match n, k with
    | (n, 0) -> 1
    | (n, k) when k=n -> 1
    | (n, k) -> bin(n-1, k-1) + bin(n-1, k)
(*
evaluation:
bin (4, 2) 
-> bin(3, 1) + bin(3, 2)
-> (bin(2, 0) + bin(2, 1)) + (bin(2, 1) + bin(2, 2))
-> (1 + (bin(1, 0) + bin(1, 1))) + (((bin(1, 0) + bin(1, 1)) + 1)
-> (1 + (1 + 1)) + ((1 + 1) + 1)
-> 6
*)

(* Exercise 2.7 Solve HR, exercise 2.9 *)
(* 
part 1 
type of f: 
int * int -> int
*)

(*
part 2:
the argument is the tuple (x, y)
*)

(*
part 3:
f(2, 3)
-> f(2-1, 2*3)
-> f(1, 6)
-> f(1-1, 1*6)
-> f(0, 6)
-> 6

// not very clear what is the mathematical meaning now, so another evaluation was made:

f(5, 3)
-> f(4, 15)     // 3 * 5
-> f(3, 60)     // 3 * (5 * 4)
-> f(2, 180)    // 3 * (5 * 4 * 3)
-> f(1, 360)    // 3 * (5 * 4 * 3 * 2)
-> f(0, 360)    // 3 * (5 * 4 * 3 * 2 * 1) equivalent to y * x!
-> 360          // f takes the final value of the second element in the tuple as its value
*)

(*
part 4:
mathematical meaning of f(x, y): y multipled by the factorial of x, denoted as y * x!
*)
