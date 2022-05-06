module ProjectInterpreter

open System

open ProjectParser
// [100.0; 112.5; 125.0; 137.5]
let rec evalOneBeat (pattern: Note list) (numBeats: int) div distance prevVal =
    let multuplier current prev =
        match current with
        | E -> 1.0
        | And ->
            match prev with
            | Num (_) -> 2.0
            | E -> 1.0
            | _ -> failwith ("Incorrect position of notes: '+' cannot come after 'a'")
        | A ->
            match prev with
            | Num (n) -> 3.0
            | E -> 2.0
            | And -> 1.0
            | _ -> failwith ("Incorrect position of notes: 'a'")
        | a -> failwith ("Something went wrong: " + string a)

    match pattern with
    | [] -> []
    | head :: tail ->
        match head with
        // the first note of the beat
        | Num (n) ->
            if int n <= numBeats then
                // calculate the distance of the next note
                distance
                :: (evalOneBeat tail numBeats div distance (Num(n)))
            else
                failwith ("Number of beats exceeds " + string numBeats + ".")
        | E ->
            let new_dist = (distance + (div * (multuplier E prevVal) * 100.0))

            new_dist
            :: (evalOneBeat tail numBeats div new_dist E)

        | And ->
            let new_dist =
                (distance
                 + (div * (multuplier And prevVal) * 100.0))

            new_dist
            :: (evalOneBeat tail numBeats div new_dist And)

        | A ->
            let new_dist = (distance + (div * (multuplier A prevVal) * 100.0))

            new_dist
            :: (evalOneBeat tail numBeats div new_dist A)


let separatePattern pattern beat =
    let rec separatePatternHelper pattern beat =
        match pattern with
        | [] -> []
        | head :: tail ->
            match head with
            | Sep -> (beat :: separatePatternHelper tail [])
            | _ -> separatePatternHelper tail (head :: beat)

    let list = separatePatternHelper pattern beat
    list |> List.map List.rev

let rec evalOnePattern pattern numBeats div distance =
    let separatedPattern = separatePattern pattern []

    List.mapi (fun i x -> evalOneBeat x numBeats div (100.0 * (float (i + 1))) (Num(uint8 1))) separatedPattern

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
        // separatePattern pattern []
        evalOnePattern pattern numBeats div 0
    // evalOneBeat pattern numBeats div 100.0 (Num(uint8 1))
    else
        failwith ("Undefined variable '" + render + "'")

// evalManyPatterns patterns envPattern render
