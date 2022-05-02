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

let TIME_SIG a b = "(4)(4) 30.5 -47.0 tsig"

let DRUM_BASS =
    "/bass{gsave
    0 exch add -47.0 hd -21.0 sd
    grestore}!"


let evalNote note props =
    match props with
    | { Time = (x, y); Division = (a, b) } ->
        match note with
        | E -> 1
        | And -> 1
        | A -> 1
        | Sep -> 0
        | Num (n) -> 1
// | E -> (1.0 / float b)
// | And -> (1.0 / (float b / 2.0))
// | A -> (1.0 / float b)
// | Sep -> 0.0
// | Num (n) -> 1.0

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

(*
    Creates a list of 1's and 0's
*)
let evalPattern pattern props =
    List.map (fun x -> (evalNote x props)) pattern

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
    | { Settings = props
        Patterns = p_data
        Render = varname } ->
        // find which pattern to render
        let expr = (findExpr p_data varname)
        // evaluate the pattern
        let pattern = (fun (Pattern (_, pattern)) -> evalPattern pattern props) expr
        // create an svg for the pattern
        let bar = evalBar pattern props
        DRUM_BASS
// (fun a b -> (a, b)) bar pattern
