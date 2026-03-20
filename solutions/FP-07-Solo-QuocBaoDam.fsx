(* Exercise 7.1 HR exercise 6.2 *)
// From the section 6.2 Symbolic differentiation:
// Type declaration
type Fexpr =    
    | Const of float 
    | X
    | Add of Fexpr * Fexpr
    | Sub of Fexpr * Fexpr
    | Mul of Fexpr * Fexpr
    | Div of Fexpr * Fexpr
    | Sin of Fexpr
    | Cos of Fexpr
    | Log of Fexpr
    | Exp of Fexpr

let ex1 = Add(Mul(Const 5.0, X),Mul(X,X))
let ex2 = Add(Mul(Const 3.0, X), Const 2.0)

// Function declaration
let rec D = function
    | Const _ -> Const 0.0
    | X -> Const 1.0
    | Add(fe,ge) -> Add(D fe, D ge)
    | Sub(fe,ge) -> Sub(D fe, D ge)
    | Mul(fe,ge) -> Add(Mul(D fe, ge), Mul(fe, D ge))
    | Div(fe,ge) -> Div(Sub(Mul(D fe,ge), Mul(fe,D ge)), Mul(ge,ge))
    | Sin fe -> Mul(Cos fe, D fe)
    | Cos fe -> Mul(Const -1.0, Mul(Sin fe, D fe))
    | Log fe -> Div(D fe, fe)
    | Exp fe -> Mul(Exp fe, D fe)

D(Sin(Mul(X, X)));;
D(Mul(Const 3.0, Exp X));;

// Conversion to textual representation
let rec toString = function
    | Const x -> string x
    | X -> "x"
    | Add(fe1,fe2) ->
        "(" + toString fe1 + ") + (" + toString fe2 + ")"           // (fe1) + (fe2)
    | Sub(fe1,fe2) ->
        "(" + toString fe1 + ") - (" + toString fe2 + ")"
    | Mul(fe1,fe2) ->
        "(" + toString fe1 + ") * (" + toString fe2 + ")"
    | Div(fe1,fe2) ->
        "(" + toString fe1 + ") / (" + toString fe2 + ")"
    | Sin fe -> "sin(" + toString fe + ")"          // sin(fe)
    | Cos fe -> "cos(" + toString fe + ")"
    | Log fe -> "log(" + toString fe + ")"
    | Exp fe -> "exp(" + toString fe + ")"

toString(Mul(Cos(Mul(X, X)),
              Add(Mul(Const 1.0, X), Mul(X, Const 1.0))));;
// val it: string = "(cos((x) * (x))) * (((1) * (x)) + ((x) * (1)))"
toString(Add(Mul(X, Mul(X, X)) , Mul(X, X)));;
// val it: string = "((x) * ((x) * (x))) + ((x) * (x))"

// now the function for "Declare an F# function with type Fexpr -> string computing the textual, postfix form of expression trees":
// (x + 7.0) has postfix form x 7.0 +
// (x+7.0)*(x−5.0) has postfix form x 7.0 + x 5.0 − *
let rec toStringPostfix = function
    | Const x -> string x
    | X -> "x"
    | Add(fe1, fe2) -> "(" + toStringPostfix fe1 + ")" + "(" + toStringPostfix fe2 + ")" + "+"
    | Sub(fe1, fe2) -> "(" + toStringPostfix fe1 + ")" + "(" + toStringPostfix fe2 + ")" + "-"
    | Mul(fe1, fe2) -> "(" + toStringPostfix fe1 + ")" + "(" + toStringPostfix fe2 + ")" + "*"
    | Div(fe1, fe2) -> "(" + toStringPostfix fe1 + ")" + "(" + toStringPostfix fe2 + ")" + "/"
    | Sin fe -> "(" + toStringPostfix fe + ")sin"
    | Cos fe -> "(" + toStringPostfix fe + ")cos"
    | Log fe -> "(" + toStringPostfix fe + ")log"
    | Exp fe -> "(" + toStringPostfix fe + ")exp"

toStringPostfix(Mul(Cos(Mul(X, X)),
              Add(Mul(Const 1.0, X), Mul(X, Const 1.0))))
toStringPostfix(Add(Mul(X, Mul(X, X)) , Mul(X, X)))
toStringPostfix (Mul(Add(X, Const 7.0), Sub(X, Const 5.0)))
toStringPostfix (Add(X, Const 7.0))


(* Exercise 7.2 HR exercise 6.8 *)
type Instruction = 
    | ADD 
    | SUB 
    | MULT 
    | DIV 
    | SIN
    | COS
    | LOG
    | EXP 
    | PUSH of float

// task 1:
// declare a type Stack for representing the stack
// declare an F# function to interpret the execution of a single instruction: intpInstr: Stack -> Instruction -> Stack

// a stack is a list of float:
type Stack = float list

// intpInstr function:
let intpInstr (stack: Stack) instr : Stack = 
    match instr with
    | ADD -> 
        match stack with 
        | a :: b :: res -> (b+a) :: res
        | _ -> failwith "not enough elements"
    | SUB -> 
        match stack with
        | a :: b :: res -> (b-a) :: res
        | _ -> failwith "not enough elements"
    | MULT ->
        match stack with
        | a :: b :: res -> (b*a) :: res
        | _ -> failwith "not enough elements"
    | DIV ->
        match stack with
        | a :: b :: res -> (b/a) :: res
        | _ -> failwith "not enough elements"
    | SIN -> 
        match stack with
        | a :: res -> sin(a) :: res
        | _ -> failwith "not enough elements"
    | COS -> 
        match stack with
        | a :: res -> cos(a) :: res
        | _ -> failwith "not enough elements"
    | LOG -> 
        match stack with
        | a :: res -> log(a) :: res
        | _ -> failwith "not enough elements"
    | EXP -> 
        match stack with
        | a :: res -> exp(a) :: res
        | _ -> failwith "not enough elements"
    | PUSH r -> r :: stack 

// task 2: 
// Declare an F# function to interpret the execution of a program: intpProg: Instruction list -> float
let intpProg (lst: Instruction list) = 
    let rec exec stack lst =        // helper function
        match lst with
        | [] ->         // base case: when there is no instructions left, get the head of the stack which is the result
            match stack with 
            | x :: _ -> x
            | _ -> failwith "empty stack"
        | instr :: res -> 
            let newStack = intpInstr stack instr
            exec newStack res
    exec [] lst     // because this task requires starting the function with an empty stack

intpProg [PUSH 3.0; PUSH 4.0; ADD]      // testing
// val it: float = 7.0                        // result

// using List.fold:
let intpProg2 (lst: Instruction list) =
    let finalStack = List.fold intpInstr [] lst
    match finalStack with
    | x :: _ -> x
    | _ -> failwith "empty stack"

// task 3:
// Declare an F# function: trans: Fexpr * float -> Instruction list
// Fexpr is the type for expression trees which were declared above
// The value of the expression trans(fe,x) is a program prg (a list of instructions) such that intpProg(prg) gives the float value of fe when X has the value x
// Hint: The instruction list can be obtained from the postfix form of the expression.
// For example: 3 + 4 => [PUSH 3; PUSH 4; ADD]

let rec trans (fe, x) = 
    match fe with
    | Const x -> [PUSH x]
    | X -> [PUSH x]     // environment = { X = x }
    | Add(fe1, fe2) -> trans (fe1, x) @ trans (fe2, x) @ [ADD]
    | Sub(fe1, fe2) -> trans (fe1, x) @ trans (fe2, x) @ [SUB]
    | Mul(fe1, fe2) -> trans (fe1, x) @ trans (fe2, x) @ [MULT]
    | Div(fe1, fe2) -> trans (fe1, x) @ trans (fe2, x) @ [DIV]
    | Sin fe -> trans (fe, x) @ [SIN]
    | Cos fe -> trans (fe, x) @ [COS]
    | Log fe -> trans (fe, x) @ [LOG]
    | Exp fe -> trans (fe, x) @ [EXP]

// let ex2 = Add(Mul(Const 3.0, X), Const 2.0), this is declared from the beginning
trans(ex2, 5)
// val it: Instruction list = [PUSH 3.0; PUSH 5.0; MULT; PUSH 2.0; ADD]


(* Exercise 7.3 HR exercise 7.2 *)
// Make signature and implementation files for a library of complex numbers with overloaded arithmetic operators (cf. Exercise 3.3)
(* implementation file *)
module Complex
type Complex = C of int * int

let make (a,b) = C(a,b)
let (~-.) C(a,b) = C(-a,-b)
let (.+.) C(a,b) C(c,d) = C(a+c, b+d) 
let (.-.) C(a, b) C(c, d) = C(a, b) .+. -. C(c, d)
let (.*.) C(a, b) C(c, d) = C(a*c - b*d, b*c + a*d)
let (./.) C(a, b) C(c, d) =
    let denom = c*c + d*d
    let inv (c, d) = 
        if c = 0 && d = 0 then 
            failwith "division by zero"
        else
            (c/denom, -d/denom)
    C(a, b) .*. inv C(c, d)

(* signature file *)
module Complex
type Complex

val make  : int * int -> Complex
val (~-.) : Complex -> Complex
val (.+.)  : Complex -> Complex -> Complex
val (.-.)  : Complex -> Complex -> Complex
val (.*.) : Complex -> Complex -> Complex
val (./.)  : Complex -> Complex -> Complex
