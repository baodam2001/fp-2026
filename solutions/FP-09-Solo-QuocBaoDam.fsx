(* Exercise 9.1 HR exercise 9.8 *)
type BinTree<'a> = 
    | Leaf
    | Node of BinTree<'a> * 'a * BinTree<'a>

// standard count function that traverses a binary tree and counts the number of nodes it contains
let rec count = function
    | Leaf -> 0
    | Node(tl, n, tr) -> count tl + count tr + 1


// first process the left subtree, update the accumulator.
// Then process the right subtree, pass the updated accumulator.
// After traversing both, add 1 for the current node.
let countA tree =
    let rec countAcc tree acc =     // tail-recursive helper
        match tree with
        | Leaf -> acc
        | Node (tl, n, lr) -> 
            let acc1 = countAcc tree acc
            let acc2 = countAcc tree acc1
            acc2 + 1
    countAcc tree 0

(* Exercise 9.2 HR exercise 9.9 *)
// leftCount is the number of nodes found so far after finishing the left subtree traversal
// acc is passed in to start counting the left subtree, leftCount is the result of that left traversal — so it's based on acc 

let rec countAC t a c =
    match t with
    | Leaf -> c a
    | Node (tl, n, tr) ->
        countAC tl a (fun leftCount ->                  // first: traverse left with the current acc
            countAC tr (leftCount + 1) c)             // then: traverse right with leftCount + 1


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

(* Exercise 9.5 - HR exercise 11.1 *)
let oddNumbers = Seq.initInfinite(fun i -> i * 2 + 1)

(* Exercise 9.6 - HR exercise 11.2 *)
let factorials =
    let rec fact = function
        | 0 -> 1
        | n -> n * fact (n - 1)
    Seq.initInfinite (fun n -> fact n)