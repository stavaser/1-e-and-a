module ProjectInterpreter

open ProjectParser

// let multuplier current prev =
//     match current with
//     | E -> 2
//     | And ->
//         match prev with
//         | Num (_) -> 2
//         | E -> 3
//         | _ -> failwith ("Incorrect position of notes: '+' cannot come after 'a'")
//     | A ->
//         match prev with
//         | Num (n) -> 3.0
//         | E -> 2.0
//         | And -> 1.0
//         | _ -> failwith ("Incorrect position of notes: 'a'")
//     | a -> failwith ("Something went wrong: " + string a)

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
    | E ->
        match next with
        | And -> 1
        | A -> 2
    | And ->
        match next with
        | A -> 1
    | A -> 1

let evalPattern (expr: Note list) _params =
    let rec evalPatternHelper beat =
        match beat with
        | [] -> []
        | head :: tail ->
            printfn "%A" head

            if List.isEmpty tail then
                []
            else
                (beam head (List.head tail))
                :: (evalPatternHelper tail)


    // let beam current prev =
//     match current with
//     | E ->
//         match prev with
//         | Num (n) -> 1
//         | _ ->
//             printfn "current: %A, prev: %A" current prev
//             0
//     | And ->
//         match prev with
//         | Num (n) -> 2
//         | A -> 1
//         | _ ->
//             printfn "current: %A, prev: %A" current prev
//             0
//     | A ->
//         match prev with
//         | Num (n) -> 3
//         | E -> 2
//         | And -> 1
//         | _ ->
//             printfn "current: %A, prev: %A" current prev
//             0
//     | _ ->
//         printfn "current: %A, prev: %A" current prev
//         0

    // let evalPattern (expr: Note list) _params =
//     let rec evalPatternHelper beat prev =
//         match beat with
//         | [] -> []
//         | head :: tail ->
//             match head with
//             | Num(n) -> (beam head prev) :: (evalPatternHelper tail head)
//             | _ -> (beam head prev) :: (evalPatternHelper tail head)


    // match head with
    // | Num (n) ->
    // | E ->
    //     (beam head (List.head tail))
    //     :: (evalPatternHelper tail)
    // | And ->
    //     (beam head (List.head tail))
    //     :: (evalPatternHelper tail)
    // | A ->
    //     (beam head (List.head tail))
    //     :: (evalPatternHelper tail)

    let list_of_beats = separatePattern expr
    printfn "list_of_beats: %A" list_of_beats

    let beats = List.mapi (fun i x -> evalPatternHelper x) list_of_beats

    beats

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
