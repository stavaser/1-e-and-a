module ProjectInterpreter

open ProjectParser
let doctype = "<?xml version=\"1.0\" standalone=\"no\"?>\n"

let prefix =
    "<svg width=\"200\" height=\"250\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">\n"

let suffix = "</svg>\n"

let circle = "<ellipse cx=\"50\" cy=\"50\" rx=\"50\" ry=\"50\" />"

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

// let rec evalShapes (ss: Shape list) (x: int) : string =
//     match ss with
//     | [] -> ""
//     | s :: ss' ->
//         let circle =
//             "<circle cx=\""
//             + (string x)
//             + "\" cy=\"75\" r=\"20\" stroke=\"rgb("
//             + (string ((x * 2) % 256))
//             + ",100,100)\" fill=\"transparent\" stroke-width=\"5\" />\n"

//         let circles = evalShapes ss' (x + 30)
//         circle + circles

// let eval2 (e: Expr) : string =
//     let str =
//         match e with
//         | Shapes ss -> evalShapes ss 25

//     doctype + prefix + str + suffix

let evalNote note =
    match note with
    | E -> 2
    | And -> 3
    | A -> 4
    | num -> 1
// | E -> "<ellipse cx=\"50\" cy=\"50\" rx=\"50\" ry=\"50\" />"
// | And -> "<ellipse cx=\"50\" cy=\"50\" rx=\"50\" ry=\"50\" />"
// | A -> "<ellipse cx=\"50\" cy=\"50\" rx=\"50\" ry=\"50\" />"
// | num -> "<ellipse cx=\"50\" cy=\"50\" rx=\"50\" ry=\"50\" />"

// let evalNotes notes =
//     match notes with
//     | [] -> ""
//     | note :: rest -> evalNote note

let evalCanvas data =
    match data with
    | { Time = (x, y); Division = (a, b) } ->
        let beats = int (b / y)
        let bar = [| for i in 1 .. (int x) -> [| for i in 1 .. (beats) -> 0 |] |]
        bar
// String.replicate (int b) (lines 102)


// let evalPatterns patterns =
//     match patterns with
//     | [] -> 0
//     // | Pattern (varname, notes) -> varname
//     | Pattern (varname, notes) :: rest -> evalNotes notes


let evalSettings settings = settings

let findExpr exprs variable =
    let isFound var = var = variable

    exprs
    |> List.find (fun (Pattern (x, _)) -> isFound x)


let eval e =
    match e with
    | { Settings = s_data
        Patterns = p_data
        Render = varname } -> (findExpr p_data varname)
// (evalCanvas s_data)
// evalSettings s_data
