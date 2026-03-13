// 'a BinTree
type 'a BinTree =
    Leaf
  | Node of 'a * 'a BinTree * 'a BinTree

// int binary tree
let intBinTree = 
  Node(43, Node(25, Node(56,Leaf, Leaf), Leaf),
                    Node(562, Leaf, Node(78, Leaf, Leaf)))

// float binary tree
let floatBinTree = 
  Node(43.0,Node(25.0, Node(56.0,Leaf, Leaf), Leaf),
            Node(562.0, Leaf, Node(78.0, Leaf,Leaf)))

// pre-order traversal
let rec preOrder tree =
  match tree with
    Leaf -> []
  | Node(n,treeL,treeR) ->
      n :: preOrder treeL @ preOrder treeR

(* Exericse 5.1 *)
let rec inOrder tree = 
    match tree with
    | Leaf -> []
    | Node(n, treeL, treeR) ->
        inOrder treeL @ n :: inOrder treeR

inOrder intBinTree

(* Exercise 5.2 *)
let rec mapInOrder f tree =
    match tree with
    | Leaf -> Leaf
    | Node(n, treeL, treeR) -> 
        Node(f n, mapInOrder f treeL, mapInOrder f treeR)

(* 
mapInOrder traverses the tree in a different order than mapPostOrder,
but the result trees are the same, they produce the same tree,
because both functions do not change the tree structure, only the values of nodes.
The final form of the tree can only be Node(n, treeL, treeR).
*)
        
(* Exercise 5.3 *)
let rec foldInOrder f acc tree =
    match tree with
    | Leaf -> acc
    | Node(n, treeL, treeR) ->
        let acc1 = foldInOrder f acc treeL
        let acc2 = f n acc1
        let acc3 = foldInOrder f acc2 treeR
        acc3

foldInOrder (fun n a -> a + n) 0.0 floatBinTree