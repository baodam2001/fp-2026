(* Exercise 8.1 HR exercise 9.1 *)
(* Consider the function g declared on Page 202 and the stack and heap after the evaluation of g 2 shown in Figure 9.2. 
Reproduce this resulting stack and heap by a systematic application of push and pop operations on the stack, 
and heap allocations that follow the step by step evaluation of g 2. *)
let xs = [1;2]

let rec g = function
| 0 -> xs
| n -> let ys = n::g(n-1)
       List.rev ys

g 2;;
//   val it : int list = [1; 1; 2; 2]

(*
evaluation:
g2
rev(2 :: g1)
rev(2 :: rev(1 :: g0))
rev(2 :: rev(1 :: [1;2]))
rev(2 :: rev([1;1;2]))
rev(2 :: [2;1;1])
rev([2;2;1;1])
[1;1;2;2] 
*)

(* 
Now is the solution:

Initial state:
Stack: 
       sf0:
       xs -> [1] -> [2]
       g  -> closure
Heap:
       [1] -> [2]      // xs

Call g2:
Push stack:
sf1: n = 2
sf0
Heap remains the same

Call g1:
Push stack:
sf2: n = 1
sf1
sf0
Heap remains the same

Call g0:
Push stack:
sf3: n = 0
sf2
sf1
sf0
Heap remains the same

Execute g0:
Return [1;2]  
Stack: Pop sf3
Heap remains the same

Execute g1:
Heap:
       [1] -> [1] -> [2]    
we have: ys = [1;1;2]
then call List.rev ys
Heap:
       [2] -> [1] -> [1] -> []
we have: ys = [2;1;1]
[1;1;2] becomes garbage
Now in Heap:
       [2] -> [1] -> [1]       // it
       [1] -> [2]         // xs   
done g1, pop sf2

Execute g2:
Heap:
       [2] -> [2] -> [1] -> [1]
we have: ys = [2;2;1;1]
then call List.rev ys
Heap: 
       [1] -> [1] -> [2] -> [2] 
we have ys = [1;1;2;2]
[2;2;1;1] becomes garbage
Now in Heap:
       [1] -> [1] -> [2] -> [2]  // it
       [1] -> [2]      // xs
done g2, pop sf1

We now have sf0, the final state.
In Stack:
       sf0:
       xs -> [1] -> [2] 
       g  -> closure
       it -> [1] -> [1] -> [2] -> [2]
In Heap:
       [1] -> [1] -> [2] -> [2]
       [1] -> [2] 
and some garbage
*) 



(* Exercise 8.2 HR exercise 9.3 *)
(* Declare an iterative solution to exercise 1.6 *)
// Recall:
let rec sum = function
| (m, 0) -> m
| (m , n) -> (m + n) + sum(m, n-1)

(* 
sum (5,3)
-> 5+3 + sum(5,2)
-> 5+3 + 5+2 + sum(5,1)
-> 5+3 + 5+2 + 5+1 + sum(5,0)
-> 5+3 + 5+2 + 5+1 + 5
-> 26
*)

// Iterative solution (tail recursion)
let rec sumA = function
| (m, 0), acc -> acc + m
| (m, n), acc -> sumA((m , n - 1), acc + m + n)

sumA ((5,3), 0)



(* Exercise 8.3 HR exercise 9.4.
Give iterative declarations of the list function List.length. 
List.length returns the number of elements in a list.
One iterative declaration is enough. *)
let rec listLengthA = function
| ([], n) -> n
| (x :: xs, n) -> listLengthA(xs, n+1)

listLengthA ([3;2;5;4;6], 0)



(* Exercise 8.4 HR exercise 9.6 *)
// tail recursive version
let rec factA = function
| (0,m) -> m
| (n,m) -> factA(n-1,n*m)

// continuation version
let rec factC = function
| 0, c -> c 1
| n, c -> factC ((n-1), (fun res -> c(n * res)))

#time

factC(50000000, id)
// Real: 00:00:01.092, CPU: 00:00:01.251, GC gen0: 1, gen1: 1, gen2: 0
// val it: int = 0

factA(50000000, 1)
// Real: 00:00:00.067, CPU: 00:00:00.067, GC gen0: 0, gen1: 0, gen2: 0
// val it: int = 0

// Compare the run time: factC is almost two times slower than factA with n = 50000000



(* Exercise 8.5 HR exercise 8.6.
This to be used in the next task *)
// Declare a fibonacchi function using while loop
// Recall:
let rec fib = function
| 0 -> 0
| 1 -> 1
| n -> fib(n-1) + fib(n-2)

fib 5

// while loop version:
let fibW n =
       if n = 0 then 0
       else if n = 1 then 1
       else 
              let mutable i = 2           // loop counter, start at 2 because we already know fibW(0) and fibW(1)
              let mutable prev = 0        // fibW(0)
              let mutable curr = 1        // fibW(1)
              while i <= n do
                     let next = prev + curr
                     prev <- curr
                     curr <- next
                     i <- i + 1
              curr

fibW 5



(* Exercise 8.6 HR exercise 9.7 *)
// Version 1: fibA: int -> int -> int -> int with two accummulating parameters
// fibA n n1 n2 = Fn, when n1 = Fn−1 and n2 = Fn−2
// always start the function with n1 = 1 and n2 = 0
let rec fibA n n1 n2 = 
       match n with
       | 1 -> n1            // n must be >= 1 so fibA (n-1) ... can make sense
       | 2 -> n1 + n2       
       | n -> fibA (n-1) (n1+n2) n1

fibA 5 1 0

// Version 2: fibC: int -> (int -> int) -> int with a continuation
let rec fibC n c = 
       match n with
       | 0 -> c 0
       | 1 -> c 1
       | n -> fibC (n - 1) (fun v1 -> 
                            fibC (n - 2) (fun v2 -> 
                                   c (v1 + v2)))

fibC 3 id
// evaluation:
// -> fibC 2 (fun v1 -> fibC 1 (fun v2 -> id(v1+v2)))
// -> fibC 1 (fun v1' ->
//            fibC 0 (fun v2' ->
//                   (fun v1 -> fibC 1 (fun v2 -> id (v1+v2))) (v1'+v2')))
// -> fibC 0 ...

// Compare these two functions using the directive #time, and compare this with the while-loop based solution of Exercise 8.6
#time
fibW 10000000
// Real: 00:00:00.014, CPU: 00:00:00.014, GC gen0: 0, gen1: 0, gen2: 0
// val it: int = -1448735941

fibA 10000000 1 0
// Real: 00:00:00.013, CPU: 00:00:00.013, GC gen0: 0, gen1: 0, gen2: 0
// val it: int = -1448735941

fibC 40 id
// Real: 00:00:02.368, CPU: 00:00:02.731, GC gen0: 130, gen1: 0, gen2: 0
// val it: int = 102334155

// fibC takes more than 2 seconds with a small n=40. When n=50, it takes a lot of time that I couldn't wait for
// Meanwhile, fibW and fibA take roughly the same amount of time with a very big n