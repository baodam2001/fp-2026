(* Question 3 - Exam June 2018 *)
(* Question 3.1 *)
let triNum = Seq.initInfinite(fun n -> (n*(n+1))/2)

// cached version
let triNumC = Seq.cache triNum

(* Question 3.2 *)
// this function filter out odd index elements, but goes into an infinite loop
// For instance filterOddIndex triNum never terminates
let rec filterOddIndex s =
    Seq.append (Seq.singleton (Seq.item 0 s))
        (filterOddIndex (Seq.skip 2 s))

// Declare your own version myFilterOddIndex similar to filterOddIndex except that it
// does not enter an infinite loop but returns the intended sequence.
// Hint: You may be inspired by Section 11.3 in HR. The sequence for myFilterOddIndex triNum is seq [0;3;10;21;...].

// each recursive step is wrapped in Seq.delay to avoid immediate computation
let rec myFilterOddIndex s =
    Seq.delay (fun () ->
        Seq.append 
            (Seq.singleton (Seq.item 0 s)) 
            (myFilterOddIndex (Seq.skip 2 s)) 
    )

// test myFilterOddIndex with the first 5 elements
myFilterOddIndex triNum |> Seq.take 5 |> Seq.toList
// val it: int list = [0; 3; 10; 21; 36]

(* Question 3.3 *)
// Declare a function seqZip of type (seq<’a> -> seq<’b> -> seq<’a * ’b>) using sequence expressions
// Seq.zip is not allowed
let rec zipSeq s1 s2 =
       seq {
            let e1 = Seq.item 0 s1          // get the first element of s1
            let e2 = Seq.item 0 s2          // get the first element of s2
            yield (e1, e2)                              // yield a sequence containing a pair (e1, e2)
            yield! zipSeq (Seq.skip 1 s1) (Seq.skip 1 s2)    // recursiveyly yield the sequences of pairs, skip the first elements of each sequence in every recursion step
        }

zipSeq triNum triNum

(* Question 4 - Exam June 2018 *)
exception FigError of string
type Point = P of double * double

type Fig =
    | Circle of Point * double
    | Line of Point * Point
    | Move of double * double * Fig
    | Combine of Fig list
    | Label of string * Fig
    | Ref of string

// The exception FigError represents an error condition from a function in the library.
// The type Point represents a point (x, y) in the two dimensional space.
// The type Fig represents the DSL for figures
// – Circle (p, r) is the circle with center p and radius r.
// – Line (p1, p2) is the line between the two points p1 and p2
// – Move (dx, dy , fig) denotes the figure obtained from fig by moving the figures contained in fig as specified by dx and dy.
// – Combine figs is the collection of figures in figs.
// – Label (lab, fig) gives the fig a name lab. We assume fig does not contain references (Ref) such that cyclic structures are avoided.
// – Ref lab references the figure with name lab assuming it exists.

(* Question 4.1 *)
// Declare an F# value rectEx of type Fig that represents a rectangle with the four points (-1,1) , (1,1), (1,-1) and (-1,-1) 
// Combine is a list, so the elements are separeted by semicolons, not commas
let rectEx = Combine [
        Line(P(-1, 1), P(1, 1)); 
        Line(P(1, 1), P(1, -1));
        Line(P(1, -1), P(-1, -1));
        Line(P(-1, -1), P(-1, 1))
    ]

// Declare an F# function rect (x1, y1) (x2, y2) of type double * double -> double * double -> Fig
let rect (x1, y1) (x2, y2) =
    Combine[
        Line(P(x1, y1), P(x2, y1));
        Line(P(x2, y1), P(x2, y2));
        Line(P(x2, y2), P(x1, y2));
        Line(P(x1, y2), P(x1, y1))
    ]

rect (-2.0,1.0) (1.0,-1.0)

(* Question 4.2 *)
// Consider the F# value figEx02 consisting of a labeled circle "c" which is referenced twice. 
// The referenced circles are moved such that we obtain a figure like the one to the right.
let figEx02 =
    Combine [
        Label("c", Circle(P(0.0,0.0), 1.0));
        Move(1.0 ,1.0, Ref "c");
        Move(2.0, 2.0, Ref "c")
    ]

// Declare an F# function buildEnv fig of type Fig -> Map<string,Fig> that 
// traverses the figure fig and builds an environment mapping labels to figures.
let buildEnv fig =
    let rec build fig m =           // helper with a map m
        match fig with
        | Label (s, f) ->               // first pattern: add fig to map and continue traversing that fig
            let m1 = Map.add s f m
            build f m1
        | Combine l -> List.fold (fun acc f -> build f acc) m l     // traverse the entire fig list
        | Move (_, _, f) -> build f m       // traverse the fig
        | _ -> m                                        // no labels, return map

    build fig Map.empty

let envEx02 = buildEnv figEx02
// val envEx02: Map<string,Fig> = map [("c", Circle (P (0.0, 0.0), 1.0))]

(* Question 4.3 *)
// Given a figure fig and an environment env mapping labels to figures, we can substitute referenced figures with the actual figures.
// Declare an F# function substFigRefs env fig of type Map<string,Fig> -> Fig -> Fig that substitues all references with actual figures. 
// As we substitute all references there is no need to keep the labels either. 
// The result figure should therefore not contain any references Ref or labels Label.
// Label("c", Circle(...)) -> Circle(...)
// Ref "c" -> Circle(...)
// Move(1.0,1.0, Ref "c") -> Move(1.0,1.0, Circle(...))
let rec substFigRefs env fig =
    match fig with
    | Label (_, f) -> f     // only return fig
    | Ref s -> Map.find s env       // only return fig which is the value of key s in map env
    | Move (d1, d2, Ref s) -> Move (d1, d2, Map.find s env)
    | Combine l -> Combine (List.map (fun i -> substFigRefs env i) l)       // recursively apply substFigRefs for every fig in the list
    | _ -> fig

let substEx02 = substFigRefs envEx02 figEx02
// val substEx02: Fig =
//   Combine
//     [Circle (P (0.0, 0.0), 1.0); Move (1.0, 1.0, Circle (P (0.0, 0.0), 1.0));
//      Move (2.0, 2.0, Circle (P (0.0, 0.0), 1.0))]

(* Question 4.4 *)
// We now assume that figures do not contain labels and references. 
// For such figures, we can remove the Move constructors by updating the positions of the circles and lines. 
// We thus obtain a figure consisting of Combine, Circle and Line constructors only.
// Declare an F# function reduceMove fig of type Fig -> Fig that updates the line and circle positions and removes the Move constructors.
let rec reduceMove fig =
    match fig with
    | Move (d1, d2, Line(P(x1, x2), P(y1, y2))) ->      // move a line to a new coordinate
        Line (P(x1+d1, x2+d2), P(y1+d1, y2+d2))
    | Move (d1, d2, Circle(P(x, y), r)) ->      // move a circle to a new coordinate
        Circle (P(x+d1, y+d2), r)
    | Combine l -> Combine (List.map(fun i -> reduceMove i) l)      // recursively move a list of figures
    | _ -> fig

let reduceEx02 = reduceMove substEx02
// val reduceEx02: Fig =
//   Combine
//     [Circle (P (0.0, 0.0), 1.0); Circle (P (1.0, 1.0), 1.0);
//      Circle (P (2.0, 2.0), 1.0)]