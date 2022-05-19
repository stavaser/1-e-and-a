module ProjectInterpreter

open ProjectParser

let separatePattern pattern =
    let rec separatePatternHelper pattern beat =
        match pattern with
        | [] -> []
        | head :: tail ->
            match head with
            | Sep -> (beat :: separatePatternHelper tail [])
            | _ -> separatePatternHelper tail (head :: beat)

    let list = separatePatternHelper pattern []
    list |> List.map List.rev

let beam current next =
    match current with
    | Num (n) ->
        match next with
        | E -> 1
        | And -> 2
        | A -> 3
        | Num (n) -> 1
    | E ->
        match next with
        | And -> 1
        | A -> 2
        | E -> 3
    | And ->
        match next with
        | A -> 1
        | And -> 2
    | A -> 1

let toABC distances drum =
    let drum_ABC drum =
        match drum with
        | HH -> "ng"
        | SN -> "c"
        | BD -> "F"

    (List.map
        (fun one_beat ->
            (List.map (fun pos -> (drum_ABC drum) + (string pos)) one_beat)
            |> String.concat "")
        distances
     |> String.concat " ")

let evalRests beats =
    let rec evalRestsHelper beats =
        match beats with
        | [] -> []
        | one_beat :: rest ->
            let sum = List.sum one_beat
            let rest_val = abs (sum - 4)
            printfn "%A" rest_val
            rest_val :: evalRestsHelper rest

    let rests = evalRestsHelper beats

    (List.map (fun value -> "z" + string value) rests)

let evalBeats beats =
    let rec evalPatternHelper beat =
        match beat with
        | [] -> []
        | head :: tail ->
            let next =
                if List.isEmpty tail then
                    head
                else
                    (List.head tail)

            (beam head next) :: (evalPatternHelper tail)

    List.map (fun x -> evalPatternHelper x) beats

let beatsToAbc beats drum =

    let drum_ABC drum =
        match drum with
        | HH -> "ng"
        | SN -> "c"
        | BD -> "F"

    (List.map (fun one_beat -> (List.map (fun pos -> (drum_ABC drum) + (string pos)) one_beat)) beats)

let combineBeatsAndRests beats rests =
    List.map2 (fun note rest -> rest :: note) beats rests


let evalPattern (expr: Note list) drum _params =
    let list_of_beats = separatePattern expr
    let beats = evalBeats list_of_beats
    let beatsABC = beatsToAbc beats drum
    let rests = evalRests beats
    let beats_and_rests = combineBeatsAndRests beatsABC rests
    printfn "beats: %A" beats
    printfn "rests: %A" rests
    printfn "beatsABC: %A" beatsABC
    printfn "beats_and_rests: %A" beats_and_rests
    printfn "separatePattern: %A" list_of_beats

    (List.map
        (fun one_beat ->
            (List.map (fun beat -> beat) one_beat)
            |> String.concat "")
        beats_and_rests
     |> String.concat " ")

// let rec transpose list =
//     if List.isEmpty (List.head list) then
//         []
//     else
//         (List.map List.head list |> List.distinct)
//         :: transpose (List.map List.tail list)

let evalBar bar _params = ""
// let rec evalBarHelper bar =
//     match bar with
//     | [] -> []
//     | head :: tail ->
//         match head with
//         | DrumPatternNotes (drum, notes) ->
//             evalPattern notes drum _params
//             + (evalBarHelper tail)
//         | DrumPatternVar (drum, var) -> (evalBarHelper tail)

// evalBarHelper bar
(*
    Evaluates AST into PostScript
*)
let eval
    { Settings = settings
      Patterns = patterns
      Bars = bars
      Snippets = snippets
      Render = render }
    =
    // get values of the settings
    let ((numBeats, beatValue), div, title, subtitle) =
        (fun { Time = (a, b)
               Division = (_, y)
               Title = title
               Subtitle = subtitle } -> (int a, int b), float y, title, subtitle)
            settings

    let _params = (numBeats, beatValue, div)
    // printfn "%A, %A" title subtitle
    // printfn "%A" bars
    let header =
        "X:1 \nM:"
        + string numBeats
        + "/"
        + string beatValue
        + "\n"
        + "L: 1/"
        + string div
        + "\nU:n=!style=x! \nK:perc \nV:ALL stem=up"

    // create pattern environment
    let envPattern =
        patterns
        |> List.map (fun (Pattern (id, data)) -> id, data)
        |> Map.ofSeq

    // create bar environment
    let envBar =
        bars
        |> List.map (fun (Bar (id, data)) -> id, data)
        |> Map.ofSeq

    // create snippet environment
    let envSnippet =
        snippets
        |> List.map (fun (Snippet (id, data)) -> id, data)
        |> Map.ofSeq

    // render value is a pattern
    if envPattern.ContainsKey render then
        let expr = envPattern.Item render
        let result = evalPattern expr HH _params
        printfn "%A" expr
        printfn "result: %A" result

        // construct PostScript
        header
    // render value is a bar
    elif envBar.ContainsKey render then
        let expr = envBar.Item render
        let result = evalBar expr _params
        printfn "%A" expr
        printfn "%A" result
        header

    // render value is a snippet
    elif envSnippet.ContainsKey render then
        let expr = envSnippet.Item render
        printfn "%A" expr
        header
    else
        failwith ("Undefined variable '" + render + "'")
