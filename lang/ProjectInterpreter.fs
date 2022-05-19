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

let fillPattern pattern =
    let example =
        [ (Num(uint8 1))
          E
          And
          A
          Sep
          (Num(uint8 2))
          E
          And
          A
          Sep
          (Num(uint8 3))
          E
          And
          A
          Sep
          (Num(uint8 4))
          E
          And
          A
          Sep ]

    let rec helper example pattern =
        match example, pattern with
        | [], [] -> []
        | ex_head :: ex_tail, head :: tail ->
            if ex_head = head then
                head :: (helper ex_tail tail)
            else
                Empty :: (helper ex_tail (head :: tail))

    helper example pattern


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

let beam current next =
    printfn "current: %A, next: %A" current next

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

// [ [ Num 1uy; Empty; And; Empty ]
//   [ Num 2uy; Empty; And; Empty ]
//   [ Num 3uy; Empty; And; Empty ]
//   [ Num 4uy; Empty; And; Empty ] ]
// let evalBeats beats =
//     let rec evalPatternHelper beat empties is_new_note =
//         match beat with
//         | [] -> []
//         | head :: tail ->
//             if head = Empty && is_new_note then
//                 0 :: (evalPatternHelper tail 1 is_new_note)
//             elif head = Empty && not is_new_note then
//                 0
//                 :: (evalPatternHelper tail (empties + 1) is_new_note)
//             else
//                 let next =
//                     if List.isEmpty tail then
//                         head
//                     else
//                         (List.head tail)

//                 if next = Empty then
//                     (empties + 1)
//                     :: (evalPatternHelper tail (empties + 1) (not (next = head)))
//                 else
//                     (beam head next)
//                     :: (evalPatternHelper tail 1 is_new_note)

//     List.map (fun x -> evalPatternHelper x 1 true) beats

let evalBeats beats =
    let rec getNonEmpty list =
        match list with
        | [] -> None
        | head :: tail ->
            if head = Empty then
                getNonEmpty tail
            else
                Some(head)

    let rec evalPatternHelper beat =
        match beat with
        | [] -> []
        | head :: tail ->
            if head = Empty then
                0 :: (evalPatternHelper tail)
            else
                let next =
                    if List.isEmpty tail then
                        head
                    else
                        (List.head tail)

                if next = Empty then
                    let new_next = getNonEmpty tail

                    match new_next with
                    | None -> 0 :: evalPatternHelper tail
                    | Some (x) -> (beam head x) :: evalPatternHelper tail

                else
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
    let filled_pattern = fillPattern expr
    let list_of_beats = separatePattern filled_pattern
    let beats = evalBeats list_of_beats
    let beatsABC = beatsToAbc beats drum
    let rests = evalRests beats
    let beats_and_rests = combineBeatsAndRests beatsABC rests
    printfn "filled_pattern: %A" filled_pattern
    printfn "separatePattern: %A" list_of_beats
    printfn "beats: %A" beats
    printfn "beatsABC: %A" beatsABC
    printfn "rests: %A" rests
    printfn "beats_and_rests: %A" beats_and_rests
    printfn "\n\n\n"

    beats_and_rests


// let rec transpose list =
//     if List.isEmpty (List.head list) then
//         []
//     else
//         (List.map List.head list |> List.distinct)
//         :: transpose (List.map List.tail list)

let evalBar bar _params =
    let rec evalBarHelper bar =
        match bar with
        | [] -> []
        | head :: tail ->
            match head with
            | DrumPatternNotes (drum, notes) ->
                evalPattern notes drum _params
                :: (evalBarHelper tail)
            | DrumPatternVar (drum, var) -> (evalBarHelper tail)

    evalBarHelper bar

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
        let result = evalPattern expr HH _params

        let ABC =
            (List.map
                (fun one_beat ->
                    (List.map (fun beat -> beat) one_beat)
                    |> String.concat "")
                result
             |> String.concat " ")

        printfn "%A" expr
        printfn "result: %A" result

        // construct PostScript
        header + ABC
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
