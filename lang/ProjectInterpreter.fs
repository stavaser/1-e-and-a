module ProjectInterpreter

open System.IO

open ProjectParser


let lines width =
    let prefix =
        "<svg width=\""
        + (string width)
        + "\" height=\"144\" viewBox=\"0 0 "
        + (string width)
        + " 144\" fill=\"none\" xmlns=\"http://www.w3.org/2000/svg\">"

    let path =
        "<path fill-rule=\"evenodd\" clip-rule=\"evenodd\" d=\"M "
        + (string width)
        + " 21 H 1 V 23 H "
        + (string width)
        + " V 21 Z M "
        + (string width)
        + " 45 H 1 V 47 H "
        + (string width)
        + " V 45 Z M 1 69 H "
        + (string width)
        + " V 71 H 1 V 69 Z M "
        + (string width)
        + " 93 H 1 V 95 H "
        + (string width)
        + " V 93 Z M 1 117 H "
        + (string width)
        + " V 119 H 1 V 117 Z\" fill=\"black\"/>"

    let suffix = "</svg>\n"
    prefix + path + suffix

let STEM_LEN = 30

let TIME_SIG (a, b) =
    "("
    + string a
    + ")("
    + string b
    + ") 30.5 -47.0 tsig\n"

let LINE = "newline"

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


let evalBar pattern data =
    match data with
    | { Time = (x, y); Division = (a, b) } ->
        let beats = int (b / y)
        let bar = [| for i in 1 .. (int x) -> [| for i in 1 .. (beats) -> 0 |] |]
        let width = (beats * (int y)) * 50
        (lines width)
// String.replicate (int b) (lines 300)

let evalNote note (time, div) =
    match note with
    | E -> 2
    | And -> 3
    | A -> 4
    | Sep -> 1
    | Num (n) -> 1
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
    Creates a list of 1's and 0's
*)
let evalPattern pattern (time, div) : string =
    let notelist = List.map (fun x -> (evalNote x (time, div))) pattern
    printfn "%A" notelist
    // let width = (fun (a, b) -> (float b) + 50.0) div
    // 1/16 =
    let width = (fun (a, b) -> (float a / float b)) div

    List.mapi
        (fun i x ->
            // let temp =
            (DRUM_BASS(
                (float x) * (100.0 / (float x))
                + ((float i) * width * 100.0)
            )))
        notelist
    |> String.concat "\n"

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
let eval e =
    match e with
    | { Settings = { Time = time; Division = div }
        Patterns = p_data
        Render = varname } ->
        // find which pattern to render
        let expr = (findExpr p_data varname)
        // evaluate the pattern
        let pattern = (fun (Pattern (_, pattern)) -> evalPattern pattern (time, div)) expr
        // create an svg for the pattern
        // let bar = evalBar pattern props
        LINE + TIME_SIG(time) + pattern
// (fun a b -> (a, b)) bar pattern
