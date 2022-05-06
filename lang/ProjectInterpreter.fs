module ProjectInterpreter

open System

open ProjectParser
// [100.0; 112.5; 125.0; 137.5]
let rec evalOnePattern (pattern: Note list) (numBeats: int) div distance prevVal =
    let multuplier prev =
        match prev with
        | Num (n) -> 1.0
        | E -> 0.5
        | And -> 0.5
        | A -> 0.5

    match pattern with
    | [] -> []
    | head :: tail ->
        match head with
        // the first note of the beat
        | Num (n) ->
            if int n <= numBeats then
                // calculate the distance of the next note
                distance
                :: (evalOnePattern tail numBeats div distance (Num(n)))
            // let new_dist = (distance + (float div * 100.0))
            // let new_result = new_dist :: result
            // (evalOnePattern tail numBeats div new_dist new_result)
            else
                failwith ("Number of beats exceeds " + string numBeats + ".")
        | E ->
            let new_dist = (distance + (div * (multuplier prevVal) * 100.0))

            new_dist
            :: (evalOnePattern tail numBeats div new_dist E)

        | And ->

            let new_dist = (distance + (div * (multuplier prevVal) * 100.0))

            new_dist
            :: (evalOnePattern tail numBeats div new_dist And)
        | A ->
            let new_dist = (distance + (div * (multuplier prevVal) * 100.0))

            new_dist
            :: (evalOnePattern tail numBeats div new_dist A)

let rec evalManyPatterns (patterns: Pattern list) =
    match patterns with
    | [] -> "patterns"
    | head :: tail ->

        printfn "%A" head
        "head"

// let createPatternEnv patterns =
//   match patterns with
//   | [] -> None
//   | head::tail -> patterns


let eval
    { Settings = settings
      Patterns = patterns
      Bars = bars
      Render = render }
    =
    // get values of the settings
    let ((numBeats, beatValue), div) =
        (fun { Time = (a, b); Division = (_, y) } -> (int a, int b), 1.0 / float y) settings

    let envPattern =
        patterns
        |> List.map (fun (Pattern (id, data)) -> id, data)
        |> Map.ofSeq

    if envPattern.ContainsKey render then
        let pattern = envPattern.Item render
        evalOnePattern pattern numBeats div 100.0 (Num(uint8 1))
    else
        failwith ("Undefined variable '" + render + "'")

// evalManyPatterns patterns envPattern render
