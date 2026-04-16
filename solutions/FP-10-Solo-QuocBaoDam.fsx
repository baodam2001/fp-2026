(* Exercise 10.1 Do assignment 1 in exam set from June 2018 *)

// We represent the heap with the below polymorphic datatype where empty nodes are represented by EmptyHP
type Heap<'a when 'a: equality> =
    | EmptyHP
    | HP of 'a * Heap<'a> * Heap<'a>

(* Question 1.1 *) 

(* Declare a value ex3 representing the binary tree shown in example 3 above. *)
let ex3 = HP(1, HP(2, HP(3, EmptyHP, EmptyHP), HP(5, EmptyHP, EmptyHP) ), 
                            HP(4, EmptyHP, EmptyHP))

(* Write the type of the value ex3. Explain why the type is either monomorphic or polymorphic *)
// Heap<int>
// The type is monomorphic, because 'a is set to be an int

(* Declare a value empty representing an empty heap, i.e. a binary tree with only one empty root
node. The type of the empty value is empty : Heap<’a> when ’a : equality. *)
let empty = EmptyHP

(* Declare an F# exception named HeapError that can be used to signal an error condition from a function on heaps. 
The exception should carry a string to be used to describe the error. *)
exception HeapError of string 

(* Question 1.2 *)
(* Declare a function
isEmpty : Heap<’a> -> bool when ’a : equality
that returns true if a heap is the empty heap. For instance isEmpty empty returns true. The value empty is defined above. *)
let isEmpty = function
| EmptyHP -> true
| _ -> false

isEmpty empty

(* The size h of a heap h is the number of non–empty nodes in the binary tree. 
Declare a function size : Heap<’a> -> int when ’a : equality
that returns the size of a heap. For instance, size ex3 returns 5. *)
let rec size = function
| EmptyHP -> 0
| HP(_, hl, hr) -> 1 + size hl + size hr

size ex3

(* Declare a function find h of type
find : Heap<’a> -> ’a when ’a : equality
that returns the minimum value in a non–empty heap, i.e. the root value. For instance find ex3 returns 1. *)
let rec find = function
| EmptyHP -> raise (HeapError "Heap is empty.")
| HP(value, hl, hr) -> value

find ex3

(* Declare a function chkHeapProperty h of type
chkHeapProperty : Heap<’a> -> bool when ’a : comparison
that returns true if the heap h fulfils the heap property and otherwise false. The empty heap by
definition fulfils the heap property. For instance chkHeapProperty ex3 returns true. *)
let rec chkHeapProperty h = 
    let checkNode parentValue childHeap =       // helper to check the property
        match childHeap with
        | EmptyHP -> true
        | HP(childValue, _, _) -> parentValue <= childValue
    
    match h with
    | EmptyHP -> true
    | HP(value, heapLeft, heapRight) -> 
        checkNode value heapLeft && checkNode value heapRight       // use the helper
        && chkHeapProperty heapLeft && chkHeapProperty heapRight            // recursively check child heaps
        // when all of the above conditions are true -> return true

chkHeapProperty ex3
chkHeapProperty empty

(* Question 1.3 *)

(* Declare a function map f h of type
map : (’a -> ’b) -> Heap<’a> -> Heap<’b>
when ’a : equality and ’b : equality
where map f h returns the heap where the function f has been applied on all values in the heap h.
You decide, but must explain, what order the function f is applied to the values in the heap. 
For instance map ((+)1) ex3 returns the heap with all values in ex3 increased by one *)

// I will make a pre-order traversal, which first process the node -> left child -> right child
// The order traversal does not change the final result anyway, since there is no side effect like printing.
let rec map f h = 
    match h with
    | EmptyHP -> EmptyHP
    | HP(value, hl, hr) -> HP(f value, map f hl, map f hr)

map ((+)1) ex3

(* The heap ex3 fulfils the heap property. 
Give an example of a function f such that mapping f on all values in ex3 gives a new heap that does not fulfil the heap property. 
Given your definition of f, show that chkHeapProperty (map f ex3) returns false *)
// An example could be: let f n = -n
// use it as an anonymous function:
let ex3' = map (fun n -> -n) ex3
chkHeapProperty ex3'
