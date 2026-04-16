(* Exercise 9.1 HR exercise 9.8 *)
(* Develop a version of the counting function for binary trees
      countA: int -> BinTree<’a> -> int
that makes use of an accumulating parameter. Observe that this function is not tail recursive. *)
type BinTree<'a> = 
    | Leaf
    | Node of BinTree<'a> * 'a * BinTree<'a>

// example:
let intBinTree = Node
                    (Node (Node (Leaf, 56, Leaf), 25, Leaf), 43,
                    Node (Leaf, 562, Node (Leaf, 78, Leaf)))

// fixed countA (accumulator version)
let rec countA acc tree =
    match tree with
    | Leaf -> acc
    | Node (tl, _, tr) ->
        let acc1 = countA acc tl
        let acc2 = countA acc1 tr
        acc2 + 1

countA 0 intBinTree

(* Exercise 9.2 HR exercise 9.9 *)
// leftCount is the number of nodes found so far after finishing the left subtree traversal
// acc is passed in to start counting the left subtree, leftCount is the result of that left traversal — so it's based on acc 

let rec countAC t a c =
    match t with
    | Leaf -> c a
    | Node (tl, n, tr) ->
        countAC tl a (fun leftCount ->                  // first: traverse left with the current acc
            countAC tr (leftCount + 1) c)             // then: traverse right with leftCount + 1

countAC intBinTree 0 id

(* Exercise 9.3 HR exercise 9.10 *)
let rec bigListK n k =
    if n=0 then k []
    else bigListK (n-1) (fun res -> 1::k(res));;

bigListK 300000 id

// evaluation example:
// bigListK 3 id
// -> bigListK 2 (fun res -> 1 :: id(res)) 
// -> bigListK 1 (fun res -> 1 :: (1 :: id(res)))
// -> bigListK 0 (fun res -> 1 :: (1 :: (1 :: id(res))))
// -> (fun res -> 1 :: (1 :: (1 :: id(res)))) [] // when n = 0 then res = []
// -> [1;1;1]

// each recursive call creates a new closure fun res -> 1 :: k res
// and these are nested, not executed immediately, so they accumulate in memory
// bigListK 300000 id builds up 300000 pending function calls and executes all at once 
// => stack overflow

(* Exercise 9.4 HR exercise 9.11 *)
(* Declare tail-recursive functions leftTree and rightTree. 
By use of leftTree it should be possible to generate a big unbalanced tree to the left containing n + 1 values in the nodes 
so that n is the value in the root, n − 1 is the value in the root of the left subtree, and so on. 
All subtree to the right are leaves. Similarly, using rightTree it should be possible to generate a big unbalanced tree to the right.
1. Use these functions to show the stack limit when using count and countA from Exercise 9.8.
2. Use these functions to test the performance of countC and countAC from Exercise 9.9. *)

// count and countC from the book
let rec count = function
      | Leaf -> 0
      | Node(tl,n,tr) -> count tl + count tr + 1

let rec countC t c =
    match t with
    | Leaf -> c 0
    | Node(tl,n,tr) ->
        countC tl (fun vl -> countC tr (fun vr -> c(vl+vr+1)))

// leftTree - normal version
// let rec leftTree n = 
//     match n with
//     | n when n < 0 -> Leaf
//     | _ -> Node(leftTree (n - 1), n, Leaf)

// leftTree - tail-recursive version
// acc is the tree is built.
// initially, acc is a Leaf, we build the tree from here.
// in each step, create new root i, then attached acc to the left of i, the right child is always a Leaf.
// i is gradually decreased, then return acc (the completed tree) when i < 0.
let leftTree n =
    let rec aux i acc =
        match i with
        | i when i < 0 -> acc
        | _ -> aux (i - 1) (Node(acc, i, Leaf))
    aux n Leaf

// example:
let leftTree1 = leftTree 2
// aux 2 Leaf
// -> aux 1 (Node(Leaf, 2, Leaf)) 
// -> aux 0 (Node(Node(Leaf, 2, Leaf), 1, Leaf)
// -> aux -1 (Node(Node(Node(Leaf, 2, Leaf), 1, Leaf), 0, Leaf), -1, Leaf)
// -> return acc, which is Node(Node(Node(Leaf, 2, Leaf), 1, Leaf), 0, Leaf)

// show stack limit when using count and countA 
count (leftTree 150000)
countA 0 (leftTree 150000)
// countA and count work fine with leftTree 100000
// but with leftTree 150000, both functions cause stack overflow
// This is because both functions are not tail-recursive and therefore build up a call stack proportional to the height of the tree

// test performance of countC and countAC 
#time
countC (leftTree 100000) id
// Real: 00:00:00.013, CPU: 00:00:00.012, GC gen0: 0, gen1: 0, gen2: 0
countAC (leftTree 100000) 0 id
// Real: 00:00:00.007, CPU: 00:00:00.006, GC gen0: 0, gen1: 0, gen2: 0
// countAC is two times faster than countC 

// similarly, we have rightTree
let rightTree n =
    let rec aux i acc =
        match i with
        | i when i < 0 -> acc
        | _ -> aux (i - 1) (Node(Leaf, i, acc))
    aux n Leaf

(* Exercise 9.5 - HR exercise 11.1 *)
let oddNumbers = Seq.initInfinite(fun i -> i * 2 + 1)

(* Exercise 9.6 - HR exercise 11.2 *)
let factorials =
    let rec fact = function
        | 0 -> 1
        | n -> n * fact (n - 1)
    Seq.initInfinite (fun n -> fact n)