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
            let sum = List.fold (fun acc (elem, _) -> acc + elem) 0 one_beat
            let rest_val = abs (sum - 4)
            printfn "%A" rest_val
            rest_val :: evalRestsHelper rest

    let rests = evalRestsHelper beats

    (List.map (fun value -> "z" + string value) rests)

let beam current next =
    // printfn "current: %A, next: %A" current next

    match current with
    | Num (n) ->
        let value =
            match next with
            | E -> 1
            | And -> 2
            | A -> 3
            | Num (n) -> 1

        (value, 1)
    | E ->
        let value =
            match next with
            | And -> 1
            | A -> 2
            | E -> 3

        (value, 2)

    | And ->
        let value =
            match next with
            | A -> 1
            | And -> 2

        (value, 3)
    | A -> (1, 4)

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

    (List.map (fun one_beat -> (List.map (fun (pos, order) -> ((drum_ABC drum) + (string pos)), order) one_beat)) beats)

let combineBeatsAndRests beats rests =
    List.map2
        (fun notes rest ->
            let (value, order) = List.head notes

            (rest :: value, order))
        beats
        rests


let evalPattern expr _params =
    // let list_of_beats = separatePattern expr
    let beats = evalBeats expr
    // let beatsABC = beatsToAbc beats drum
    // let rests = evalRests beats
    // let beats_and_rests = combineBeatsAndRests beatsABC rests
    // printfn "separatePattern: %A" list_of_beats
    printfn "beats: %A" beats
    // printfn "beatsABC: %A" beatsABC
    // printfn "rests: %A" rests
    // printfn "beats_and_rests: %A" beats_and_rests
    printfn "\n\n\n"

    beats


let rec transpose list =
    if List.isEmpty (List.head list) then
        []
    else
        (List.map List.head list |> List.distinct)
        :: transpose (List.map List.tail list)

let rec combinePatterns patterns =
    match patterns with
    | [] -> []
    | one_beat :: beat_rest ->
        let map =
            (one_beat |> List.concat)
            |> List.groupBy (fun (value, order) -> order)
            |> List.sortBy (fun (order, value) -> order)
            |> List.map (fun (order, list) -> order, List.map (fun (drum, order) -> drum) list)

        map :: combinePatterns beat_rest

// [[(1, ["ng2"; "F1"]); (2, ["F1"]); (3, ["ng2"; "F1"]); (4, ["F1"])];
//  [(1, ["ng1"; "F3"]); (2, ["ng3"]); (4, ["F1"])];
//  [(1, ["ng1"; "F3"]); (2, ["ng2"]); (4, ["ng1"; "F1"])];
//  [(1, ["ng2"; "F3"]); (3, ["ng2"]); (4, ["F1"])]]
let manyPatternsToString patterns =
    patterns
    |> List.map (fun (list) ->
        (fun (_, notes) ->
            notes
            |> List.map (fun (drum) -> "[" + (drum |> String.concat "") + "]")) (list |> List.unzip))
    |> List.map (fun (beat) -> beat |> String.concat "")
    |> String.concat " "


let beatSort beat =
    let indexed =
        List.map
            (fun note ->
                match note with
                | Num (n) -> (note, 1)
                | E -> (note, 2)
                | And -> (note, 3)
                | A -> (note, 4))
            beat

    indexed
    |> List.sortBy (fun (note, index) -> index)
    |> List.map (fun (note, index) -> note)

let separatePattern2 drum pattern =
    let notes_with_drums = List.map (fun x -> (x, drum)) pattern

    let rec separatePatternHelper pattern beat =
        match pattern with
        | [] -> []
        | head :: tail ->
            match head with
            | (Sep, _) -> (beat :: separatePatternHelper tail [])
            | _ -> separatePatternHelper tail (head :: beat)

    let list = separatePatternHelper notes_with_drums []
    list |> List.map List.rev


let evalBar bar _params =
    let rec evalBarHelper bar =
        match bar with
        | [] -> []
        | head :: tail ->
            match head with
            | DrumPatternNotes (drum, notes) ->
                let separated = separatePattern2 drum notes
                separated :: (evalBarHelper tail)
            | DrumPatternVar (drum, var) -> (evalBarHelper tail)

    let bars = evalBarHelper bar

    let transposed =
        bars
        |> transpose
        |> List.map (fun x -> x |> List.concat)
        |> List.map (fun note_drum_pair ->
            note_drum_pair
            |> List.groupBy (fun (note, drum) -> note))
        |> List.map (fun one_beat ->
            one_beat
            |> List.map (fun (note, note_drum_pairs) -> note_drum_pairs |> List.unzip)
            |> List.map (fun (notes, drums) -> (List.head notes), drums))


    // |> List.groupBy (fun ((Note (note)), (Drum (drum))) -> note)

    // |> List.map (fun x -> x |> List.concat |> List.distinct |> beatSort)

    // let evaluatedBeats = evalBeats transposed

    // let transposed = transpose bars
    // let combined = combinePatterns transposed
    // let string = manyPatternsToString combined
    printfn "bars: %A" bars
    printfn "transposed: %A" transposed
    // printfn "evaluatedBeats: %A" evaluatedBeats
    // printfn "transposed: %A" transposed
    // printfn "combined: %A" combined
    // printfn "string: %A" string
    []


// [ [ [ "z0"; "ng2"; "ng2" ]
//     [ "z0"; "ng2"; "ng2" ]
//     [ "z0"; "ng2"; "ng2" ]
//     [ "z0"; "ng2"; "ng2" ] ]
//   [ [ "z0"; "F1"; "F1"; "F1"; "F1" ]
//     [ "z0"; "F3"; "F1" ]
//     [ "z0"; "F3"; "F1" ]
//     [ "z0"; "F3"; "F1" ] ] ]
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
        + "\nU:n=!style=x! \nK:perc \nV:ALL stem=up\n"

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
        // let result = evalPattern expr HH _params

        // let ABC =
        //     (List.map
        //         (fun one_beat ->
        //             (List.map (fun (beat, _) -> beat) one_beat)
        //             |> String.concat "")
        //         result
        //      |> String.concat " ")

        printfn "%A" expr
        // printfn "result: %A" result

        // construct PostScript
        header
    // render value is a bar
    elif envBar.ContainsKey render then
        let expr = envBar.Item render
        let result = evalBar expr _params
        // printfn "%A" expr
        printfn "%A" result
        header

    // render value is a snippet
    elif envSnippet.ContainsKey render then
        let expr = envSnippet.Item render
        printfn "%A" expr
        header
    else
        failwith ("Undefined variable '" + render + "'")
