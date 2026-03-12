(* Exercise 4.1 *)
let explode (s: string) = 
    let arr = s.ToCharArray()
    List.ofArray arr

// shorter version:
// let explode (s:string) = List.ofArray (s.ToCharArray())

// s.Chars to get the first element of the array containing the characters of s
// s.Remove(0, 1) to remove characters from index 0, 1 is the number of characters to be removed
let rec explode2 (s: string) = 
    match s with
    | "" -> []
    | _ -> s.Chars(0) :: explode2 (s.Remove(0,1))

(* Exercise 4.2 *)
let implode (s: char list) = 
    List.foldBack (fun c acc -> string c + acc ) s ""

let implodeRev (s: char list) = 
    List.fold (fun acc c -> string c + acc) "" s

(* Exercise 4.3 *)
let toUpper s = implode(List.map System.Char.ToUpper (explode s))

let toUpper1 s = implode (((explode >> List.map System.Char.ToUpper)) s)

let toUpper2 s = (List.map System.Char.ToUpper << explode) s |> implode

(* Exericse 4.4 *)
// turn s into a list of chars by explode defined above
// then use List.rev that reverse a list to check if s is palindrome
let palindrome s = 
    let chars = explode s
    chars = List.rev chars

(* Exercise 4.5 *)
let rec ack = function
| 0, n -> n+1
| m , 0 -> ack (m-1, 1)
| m, n -> ack(m-1, ack(m, n-1))

(* 
through experiments:
ack(0,n) -> n+1
ack(1,n) -> n+2
ack(2,n) -> 2n+3
ack(3,n) -> 2^(n+3) - 3 

so we have:
ack(3,11) -> 2^14 - 3 
*)

