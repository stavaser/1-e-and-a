module ProjectInterpreter

open System.IO

open ProjectParser

// let STEM_LEN length = "/stemlen" + string (-length) + "def\n"
// let BAR_WIDTH width = "/bar_width" + string width + " def\n"

let TIME_SIG (a, b) =
    "("
    + string a
    + ")("
    + string b
    + ") 30.5 -47.0 tsig\n"

let LINE width = string width + " newline\n"

let DRUM_BASS distance = string distance + " bass\n"



// | E -> 2
// | And -> 3
// | A -> 4
// | Sep -> 0
// | Num (n) -> 1/
// [1/4,2,3,4,0,1,3,0,1,0,1]



// | E -> "<ellipse cx=\"50\" cy=\"50\" rx=\"50\" ry=\"50\" />"
// | And -> "<ellipse cx=\"50\" cy=\"50\" rx=\"50\" ry=\"50\" />"
// | A -> "<ellipse cx=\"50\" cy=\"50\" rx=\"50\" ry=\"50\" />"
// | num -> "<ellipse cx=\"50\" cy=\"50\" rx=\"50\" ry=\"50\" />"


let evalBar pattern data = data
// match data with
// | { Time = (x, y); Division = (a, b) } ->
//     let beats = int (b / y)
//     let bar = [| for i in 1 .. (int x) -> [| for i in 1 .. (beats) -> 0 |] |]
//     let width = (beats * (int y)) * 50
//     (lines width)
// String.replicate (int b) (lines 300)

let evalNote note =
    // let mult = (fun (x, y) -> (float len) / (float x)) time
    // let width = (fun (a, b) -> (float a / float b) * mult) div
    // // printfn "%A, %A" mult width

    // List.mapi (fun i x -> (DRUM_BASS((float i) * mult))) notelist
    // // |> String.concat "\n"

    // match note with
    // | E -> float beat + (1.0 / float y)
    // | And -> float beat + (1.0 / float y)
    // | A -> float beat + (1.0 / float y)
    // | Sep -> 0
    // | Num (n) -> float n
    match note with
    | E -> 1.0
    | And -> 1.0
    | A -> 1.0
    | Sep -> 0
    | Num (n) -> 1.0
// 100 25 50 75
// match (time, div) with
//  4/4     1/16
// | (x, y), (a, b) ->
//     match note with
//     | E -> (1.0 / float b)
//     | And -> (1.0 / (float b / 2.0))
//     | A -> (1.0 / float b)
//     | Sep -> 0.0
//     | Num (n) -> 1.0
// pattern a: 1 + | 2 + | 3 + | 4 +
//   1/4
// [[0.25, 0.25, 0.25, 0.25], [1], [1], [1]]
//  . . . . . _ _ _ . _ _ _ . _ _ _
(*
    i think this is what i need to do:
    i should recursively pass the current distance and add
    div to it multiplied by the note value (1, e, and, a)
    BUT HOW I HAVE NO IDEA
*)
let evalDiv div = (fun (a, b) -> float a / float b) div

let evalPattern pattern (time, div) len =
    let frac_div = (evalDiv div)
    printfn "%A" pattern
    let result = []

    let full_measure =
        [ Num(1)
          E
          And
          A
          Num(2)
          E
          And
          A
          Num(3)
          E
          And
          A
          Num(4)
          E
          And
          A ]

    let rec helper pattern distance res =

        match pattern with
        | [] -> DRUM_BASS distance + res
        | note :: rest ->
            // printfn "%A" distance
            let new_dist = (distance + frac_div * 100.0 * (evalNote note))
            let new_res = (DRUM_BASS new_dist) + res
            (helper rest new_dist new_res)

    // pattern
    // |> Seq.pairwise
    // |> Seq.map (fun (xn_1, xn) -> (xn - xn_1) / xn_1)

    // List.map (helper pattern 100.0 result) pattern
    let distances = helper pattern 100.0 ""
    distances

// List.mapi (fun x -> (DRUM_BASS x)) distances
// |> String.concat "\n"
// let notelist = List.mapi (fun i x -> (evalNote x)) pattern
// printfn "%A" notelist

// let mult = (fun (x, y) -> (float len) / (float x)) time

// List.mapi (fun i x -> (DRUM_BASS((float x) * mult))) notelist
// |> String.concat "\n"






// for note in notelist do
//     String.replicate (int b) (DRUM_BASS distance)
// let rec helper pattern distance =
//     match pattern with
//     | [] -> ""
//     | head :: tail ->
//         if head = 1 then
//             (DRUM_BASS distance)
//             + helper tail (distance + 100)
//         else
//             "\n"


// let res = helper notelist 100
// res


(*
    Dummy function
*)
let evalSettings settings = settings

(*
    Finds an expression given a list
    of expression and the variable name
*)
let findExpr exprs variable =
    let isFound var = var = variable

    exprs
    |> List.find (fun (Pattern (x, _)) -> isFound x)

(*
    Returns a bar with length depending on the division
    of the beats and a list of notes that should fit in
    bar, where 0 separates notes for each beat
    So for example the list
    [1; 1; 1; 1; 0; 1; 1; 1; 1; 0; 1; 0; 1; 0]
    would mean that
    in 4/4 time and 1/16 divison
    in this one bar, there are
        - 4 16th notes
        - 4 16th notes
        - 1 4th note
        - 1 4th note
*)

// 4/4
// 4 things of len 4
// div - mimimum distacne btwn notes
let calc_bar_len time division = 400


let eval e =
    match e with
    | { Settings = { Time = time; Division = div }
        Patterns = p_data
        Render = varname } ->
        // find which pattern to render
        let expr = (findExpr p_data varname)
        // evaluate the pattern
        // create an svg for the pattern
        // let bar = evalBar pattern props
        let len = calc_bar_len time div

        let pattern =
            (fun (Pattern (_, pattern)) -> evalPattern pattern (time, div) len) expr

        LINE(len) + TIME_SIG(time) + pattern
// pattern
// (fun a b -> (a, b)) bar pattern
