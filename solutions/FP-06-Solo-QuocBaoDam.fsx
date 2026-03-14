(* Exericse 6.1 *)
type state = Map<string,int>
let update x v s = Map.add x v s

// The declaration for the abstract syntax for arithmetic expressions:
type aExp =                 (* Arithmetical expressions *)
    | N of int              (* numbers *)
    | V of string           (* variables *)
    | Add of aExp * aExp    (* addition *)
    | Mul of aExp * aExp    (* multiplication *)
    | Sub of aExp * aExp    (* subtraction *)

// semantic:
let rec A a s =
    match a with
    | N n -> n
    | V x -> Map.find x s
    | Add(a1, a2) -> A a1 s + A a2 s
    | Mul(a1, a2) -> A a1 s * A a2 s
    | Sub(a1, a2) -> A a1 s - A a2 s

// The declaration of the abstract syntax for boolean expressions:
type bExp =                 (* Boolean expressions *)
    | TT                    (* true *)
    | FF                    (* false *)
    | Eq of aExp * aExp     (* equality *)
    | Lt of aExp * aExp     (* less than *)
    | Neg of bExp           (* negation *)
    | Con of bExp * bExp    (* conjunction *)

// semantic:
let rec B b s =
    match b with
    | TT -> true
    | FF -> false
    | Eq (a1, a2) -> A a1 s = A a2 s
    | Lt (a1, a2) -> A a1 s < A a2 s
    | Neg b -> not (B b s)
    | Con (b1, b2) -> B b1 s && B b2 s

// The abstract syntax for the statements:
// two constructors were added for exercise 6.2
type stm =                      (* statements *)
    | Ass of string * aExp      (* assignment *)
    | Skip
    | Seq of stm * stm          (* sequential composition *)
    | ITE of bExp * stm * stm   (* if-then-else *)
    | While of bExp * stm       (* while *)
    | RU of bExp * stm          (* repeat until *)
    | IT of bExp * stm          (* if then *)

// semantic:
// this is also the extended version for exercise 6.2
let rec I stm s =
    match stm with
    | Ass(x,a) -> update x (A a s) s    // compute value of a, then update state
    | Skip -> s
    | Seq(stm1, stm2) -> I stm2 (I stm1 s)  // run stm1 first, get new state, then run stm2
    | ITE(b,stm1,stm2) -> 
        if B b s then I stm1 s 
        else I stm2 s
    | While(b, stm) ->                  // recursively run stm until b is false, when b is false, return statement
        if B b s then I (While(b, stm)) (I stm s) 
        else s     
    | RU(b, stm) ->                     // run stm at least once, so we get the state (I stm s). then check the condition b with that state. if correct, the loop is terminated with state (I stm s), if not crrect then repeat-until
        if B b (I stm s) then I stm s 
        else I(RU(b, stm)) (I stm s) 
    | IT(b, stm) ->                     // simply skipping in the "else" branch
        if B b s then I stm s 
        else I Skip s  

// 5 examples: 

// Example 1
let stmt0 = Ass("res",(Add(N 10, N 30)))
let state0 = Map.empty
I stmt0 state0

// Example 2
// this example uses Skip, which does nothing, the state is unchanged
let stmt1 = Skip 
let state1 = Map.ofList [("x", 24)]
I stmt1 state1

// Example 3
// this example uses Seq. 
// 3 is assigned to x, then x+2 (which is 5) is assigned to y
let stmt2 = 
    Seq(
        Ass("x", N 3),
        Ass("y", Add(V "x", N 2))
    )
let state2 = Map.empty
I stmt2 state2

// Example 4
// this example uses ITE
// since x < 10, "then" branch is run, which adds variable y with value 2 to the state3
let stmt3 =
    ITE(
        Lt(V "x", N 10),
        Ass("y", N 1),
        Ass("y", N 2)
    )

let state3 = Map.ofList [("x", 24)]
I stmt3 state3

// Example 5
// this example uses While
// while 0 < x, x is reduced by 1, the final result is ("x", 0)
let stmt4 =
    While(
        Lt(N 0, V "x"),
        Ass("x", Sub(V "x", N 1))
    )

let state4 = Map.ofList [("x", 24)]
I stmt4 state4

(* Exercise 6.2 *)
// the extended version is declared in exercise 6.1 above.

(* Exercise 6.3 *)
(*
In A, arithmetic expressions do not change the state, it only read the state.
inc(x) changes the state, it also return the new state.
That means A has new type A aExp -> state -> (int * state).
This causes rippling effect on B and I because they call A.

So, to cope with this construct, I can extend the abstract syntax by a new constructor Inc, which increases the current value of x by 1, the update the state with the new x.
The value of the expression Inc will be the new value of x.
With inc(x), the expresison evaluation changes the state, so A must have new type: aExp -> state -> (int * state).
Other expression in A like Add, Mul,... must be adjusted, like we must pass the state through each evaluation step.
For example, with Sub:
1. (v1, s1) = evaluate a1 in s
2. (v2, s2) = evaluate a2 in s1
3. return (v1 - v2, s2)
Since type of A changes, type of B is bExp -> state -> (bool * state) because the evaluation of Eq, Lt,... can has inc(x).
Statement implementation should be adjusted. For example:
Ass(x,a) -> update x (A a s) s
A a s returns an int, but with inc(x) added, it returns int * state.
The process is to evaluate a in state s first, then we have (new value, new state s1), then update x with new value to create another state: s2.
*)