module ProjectInterpreter

let doctype = "<?xml version=\"1.0\" standalone=\"no\"?>\n"

let prefix =
    "<svg width=\"200\" height=\"250\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">\n"

let suffix = "</svg>\n"

let circle = "<ellipse cx=\"50\" cy=\"50\" rx=\"50\" ry=\"50\" />"

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

let eval e = e
