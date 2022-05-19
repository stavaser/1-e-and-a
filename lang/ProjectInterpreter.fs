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

let evalPattern (expr: Note list) _params =
    let rec evalPatternHelper beat =
        match beat with
        | [] -> []
        | head :: tail ->
            printfn "%A" head

            let next =
                if List.isEmpty tail then
                    head
                else
                    (List.head tail)

            (beam head next) :: (evalPatternHelper tail)

    let list_of_beats = separatePattern expr
    let beats = List.mapi (fun i x -> evalPatternHelper x) list_of_beats

    toABC beats HH



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
        let result = evalPattern expr _params
        printfn "%A" expr
        printfn "result: %A" result

        // construct PostScript
        header
    // render value is a bar
    elif envBar.ContainsKey render then
        let expr = envBar.Item render
        printfn "%A" expr
        header

    // render value is a snippet
    elif envSnippet.ContainsKey render then
        let expr = envSnippet.Item render
        printfn "%A" expr
        header
    else
        failwith ("Undefined variable '" + render + "'")
