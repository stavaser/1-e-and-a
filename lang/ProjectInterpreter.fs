module ProjectInterpreter

open ProjectParser
open System.Collections.Generic

let MAX_LINE_WIDTH = 681.6


let TITLE str =
    "340.8 4.0 M  20.0 F2 ("
    + str
    + ")showc
    0 -19.00 T \n"

let SUBTITLE str =
    "340.8 3.2 M  16.0 F2 ("
    + str
    + ")showc
    0 -12.00 T\n"

let TIME_SIG (a, b) =
    "("
    + string a
    + ")("
    + string b
    + ") 30.5 -47.0 tsig\n"

let TRANSLATE = "0 -87.60 T \n"
let FIRST_LINE = "0 87.60 T \n"
let LINE width = string width + " newline\n"

let NEW_PAGE_START =
    "grestore showpage
    gsave 0.75 dup scale 0 1018.2 T
    67.20 0 T
    0 -22.00 T
    /y0{-47.0 add}!
    /yns0{-47.0 add}! \n"

let NEW_PAGE_END = "grestore showpage \n"

let BEAM8 (offset, len) =
    "/offset "
    + (string offset)
    + " def\n"
    + (string len)
    + " beam8\n"

let CRASH distance = string distance + " crash\n"
let RIDE distance = string distance + " ride\n"

let CLOSED_HH distance = string distance + " closedhh\n"
let OPEN_HH distance = string distance + " openhh\n"

let SNARE distance = string distance + " snare\n"

let TOM_1 distance = string distance + " tom1\n"
let TOM_2 distance = string distance + " tom2\n"

let FOOT_HH distance = string distance + " foothh\n"
let BASS distance = string distance + " bass\n"

(*

*)
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

let evalOneBeat beat numBeats div i =
    let rec evalOneBeatHelper (pattern: Note list) (numBeats: int) div distance prevVal =

        match pattern with
        | [] -> []
        // [n, e, and, a]
        | head :: tail ->
            match head with
            // the first note of the beat
            // 4/4
            | Num (n) ->
                if int n <= numBeats then
                    // calculate the distance of the next note
                    distance
                    :: (evalOneBeatHelper tail numBeats div distance (Num(n)))
                else
                    failwith ("Number of beats exceeds " + string numBeats + ".")
            | E ->
                let new_dist = (distance + (div * (multuplier E prevVal)))

                new_dist
                :: (evalOneBeatHelper tail numBeats div new_dist E)
            // 1/16
            | And ->
                let new_dist = (distance + (div * (multuplier And prevVal)))

                new_dist
                :: (evalOneBeatHelper tail numBeats div new_dist And)

            | A ->
                let new_dist = (distance + (div * (multuplier A prevVal)))

                new_dist
                :: (evalOneBeatHelper tail numBeats div new_dist A)

    evalOneBeatHelper beat numBeats div i (Num(uint8 1))

(*
    Given a list of
*)
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

let rec evalOnePattern pattern numBeats div =
    let separatedPattern = separatePattern pattern
    printfn "%A" (separatedPattern)
    // where the first note of a beat should start
    let offset = 60.0
    let note_start = (MAX_LINE_WIDTH - offset) / float numBeats
    // minimum distance between two consequtive notes
    let dist = note_start / float numBeats

    List.mapi
        (fun i x -> evalOneBeat x numBeats dist ((float i) * note_start + offset))

        separatedPattern

(* TODO *)
// [60.0; 137.7; 215.4; 293.1; 370.8; 526.2]
// let evalBeams combined_distances numBeats div =
//     let offset = 60.0
//     let note_start = (MAX_LINE_WIDTH - offset) / float numBeats

//     ""

// let rec evalBeamsHelper one_beat prev_note =
//     match one_beat with
//     | [] -> ""
//     | note :: note_rest ->
//         let len = note

//         BEAM8(note, len)
//         + (evalBeamsHelper note_rest note)

// match distances with
// | [] -> ""
// | one_beat :: rest ->
//     (evalBeamsHelper one_beat 0.0)
//     + evalBeams rest (offset + 1.0)

let createPatternPS drum pattern numBeats div =
    // 2d list of distances of notes in each beat
    let distances = evalOnePattern pattern numBeats div
    // printfn "%A" (distances)

    let drum_PS pos drum =
        match drum with
        | CC -> CRASH pos
        | RD -> RIDE pos
        | HH -> CLOSED_HH pos
        | SN -> SNARE pos
        | T1 -> TOM_1 pos
        | T2 -> TOM_2 pos
        | BD -> BASS pos

    (List.map
        (fun one_beat ->
            (List.map (fun pos -> (drum_PS pos drum)) one_beat)
            |> String.concat "\n")
        distances
     |> String.concat "\n")



let evalOneDrumPattern drum_pattern (envPattern: Map<PatternName, Note list>) numBeats div =
    match drum_pattern with
    | DrumPatternNotes (drum, notes) -> createPatternPS drum notes numBeats div
    | DrumPatternVar (drum, var) ->
        if envPattern.ContainsKey var then
            let notes = envPattern.Item var
            createPatternPS drum notes numBeats div
        else
            failwith ("Undefined variable '" + var + "'")


// let combineBeats (map: Map<int, Note list list>) current =

let rec transpose list =
    if List.isEmpty (List.head list) then
        []
    else
        (List.map List.head list |> List.distinct)
        :: transpose (List.map List.tail list)



let unwrap drum_pattern (envPattern: Map<PatternName, Note list>) =
    match drum_pattern with
    | DrumPatternNotes (drum, notes) -> separatePattern notes
    | DrumPatternVar (drum, var) ->
        if envPattern.ContainsKey var then
            let notes = envPattern.Item var
            separatePattern notes
        else
            failwith ("Undefined variable '" + var + "'")
(*
    [
        [[Num 1uy; And]; [Num 2uy; And]; [Num 3uy; And]; [Num 4uy; And]];
        [[Num 1uy; And]; [Num 2uy; And]; [Num 3uy; And]; [Num 4uy; And]];
        [[]; [Num 2uy]; []; [Num 4uy]];
        [[Num 1uy]; [Num 2uy]; [Num 3uy]; [Num 4uy; E; And; A]]
    ]

[
    [Num 1uy; And]; [Num 2uy; And]; [Num 3uy; And]; [Num 4uy; And];
    [Num 1uy; And]; [Num 2uy; And]; [Num 3uy; And]; [Num 4uy; And];
    []; [Num 2uy]; []; [Num 4uy];
    [Num 1uy]; [Num 2uy]; [Num 3uy]; [Num 4uy; E; And; A]
]

*)

// let rec beamHelper one_beat =
//     match
// let shouldAdd current prev =
//     match current with
//     | E -> 1.0
//     | And ->
//         match prev with
//         | Num (_) -> 2.0
//         | E -> 1.0
//         | _ -> failwith ("Incorrect position of notes: '+' cannot come after 'a'")
//     | A ->
//         match prev with
//         | Num (n) -> 3.0
//         | E -> 2.0
//         | And -> 1.0
//         | _ -> failwith ("Incorrect position of notes: 'a'")
//     | a -> failwith ("Something went wrong: " + string a)
let rec evalBeams distances numBeats div =
    let rec helper distances =
        match distances with
        | [] -> []
        | one_beat :: rest ->
            let unique = List.concat one_beat |> List.distinct
            printfn "unique: %A" unique

            let rec helperHelper list add =
                match list with
                | [] -> 0
                | note :: note_rest ->
                    match note with
                    | Num (n) -> add + (helperHelper note_rest (add + 1))
                    | E -> add + (helperHelper note_rest (add + 2))
                    | And -> add + (helperHelper note_rest (add + 3))
                    | A -> add + (helperHelper note_rest (add + 4))
                    | Sep -> 0

            let len = helperHelper unique 0
            len :: (helper rest)

    helper distances

// match unique with
// | [] -> ""
// | note :: note_rest ->
//     match note with
//     | Num (n) ->
//     | E -> "Num(n)"
//     | And -> "Num(n)"
//     | A -> "Num(n)"
//     | Sep -> ""



let evalManyDrumPatterns drum_patterns (envPattern: Map<PatternName, Note list>) numBeats div =
    // let map =
    //     List.mapi (fun i (DrumPatternNotes (drum, pattern)) -> separatePattern pattern) drum_patterns
    let map = List.map (fun item -> unwrap item envPattern) drum_patterns
    let distances = transpose map
    let beams = evalBeams distances numBeats div

    // let combined_distances =
    //     List.map (fun (DrumPatternNotes (drum, pattern)) -> separatePattern pattern) drum_patterns
    //     |> List.concat

    // // printfn "combined_distances: %A \n\n" combined_distances
    // printfn "drum_patterns: %A \n\n" map
    printfn "transpose: %A \n\n" distances
    printfn "beams: %A \n\n" beams

    List.map (fun expr -> (evalOneDrumPattern expr envPattern numBeats div)) drum_patterns
    |> String.concat "\n"

// [

// [[Num 1uy]; [Num 1uy]; [And]];
// [[]; [Num 2uy]; [Num 2uy; And]];
// [[]; [Num 3uy]; [Num 3uy]];
// [[Num 4uy]; [Num 4uy]; [Num 4uy]]

// ]

(*
    Given a list of bar names, evaluates them into PostScript

    params:
        bars        -   list of bar names
        envBar      -   map of bar names as keys and their patterns as values
        numBeats    -   number of beats (top number of the time signature)
        div         -   minimum division of two notes
*)
let evalManyBars bars (envPattern: Map<PatternName, Note list>) (envBar: Map<BarName, DrumPattern list>) numBeats div =
    let rec evalManyBarsHelper bars =
        match bars with
        | [] -> ""
        | bar :: tail ->
            // if the bar exists
            if envBar.ContainsKey bar then
                // contains many "drum -> pattern" assignments such as
                // sn: 1 | 2 | 3 | 4 |
                // bd: 1 e | 2 e | 3 a | 4 a |
                let drum_pattern = envBar.Item bar
                // evaluates many "drum -> pattern" assignments into PostScript
                let drums = evalManyDrumPatterns drum_pattern envPattern numBeats div

                // construct a bar
                TRANSLATE
                + LINE(MAX_LINE_WIDTH)
                + drums
                + (evalManyBarsHelper tail)
            else
                ""
        | _ -> "smth is horrible wrong in evalManyBars"

    evalManyBarsHelper bars

(*
  Given two bars merge them into one and replace duplicate drums in the old bar
*)
let evalBarDifference old_bar new_bar =
    let envDrumPatternOld =
        old_bar
        |> List.map (fun (smth) ->
            match smth with
            | DrumPatternNotes (drum, _) -> drum, smth
            | DrumPatternVar (drum, _) -> drum, smth)
        |> Map.ofSeq

    let envDrumPatternNew =
        new_bar
        |> List.map (fun (smth) ->
            match smth with
            | DrumPatternNotes (drum, _) -> drum, smth
            | DrumPatternVar (drum, _) -> drum, smth)
        |> Map.ofSeq

    let newMap =
        Map.fold (fun acc key value -> Map.add key value acc) envDrumPatternOld envDrumPatternNew

    List.map
        (fun (_, data) ->
            match data with
            | DrumPatternNotes (_, _) -> data
            | DrumPatternVar (_, _) -> data)
        (newMap |> Map.toList)

// let envDrumPatternOld =
//     old_bar
//     |> List.map (fun (DrumPatternNotes (drum, pattern)) -> drum, pattern)
//     |> Map.ofSeq

// let envDrumPatternNew =
//     new_bar
//     |> List.map (fun (DrumPatternNotes (drum, pattern)) -> drum, pattern)
//     |> Map.ofSeq




let evalRepeatChange repeat_num literals old_bar change_data numBeats div (envPattern: Map<PatternName, Note list>) =
    let bar_nums = [ for i in 1..repeat_num -> i ]

    let all_old_bars =
        (List.map (fun num -> (num, old_bar)) bar_nums)
        |> List.map (fun (num, data) -> num, data)
        |> Map.ofSeq

    let all_new_bars =
        (List.map (fun num -> (num, change_data)) literals)
        |> List.map (fun (num, data) -> num, data)
        |> Map.ofSeq

    let new_bar = evalBarDifference old_bar change_data

    let newMap =
        Map.fold (fun acc key value -> Map.add key new_bar acc) all_old_bars all_new_bars

    let bars_ast = (List.map (fun (_, data) -> data) (newMap |> Map.toList))

    let bars_ps =
        List.map
            (fun expr ->
                TRANSLATE
                + LINE(MAX_LINE_WIDTH)
                + (evalManyDrumPatterns expr envPattern numBeats div))
            bars_ast


    // printfn "%A" bars_ast

    (bars_ps |> String.concat "\n")

let evalRepeatChangeEvery
    repeat_num
    every_num
    (old_bar: DrumPattern list)
    change_data
    numBeats
    div
    (envPattern: Map<PatternName, Note list>)
    =
    let new_bar = evalBarDifference old_bar change_data

    let bar_nums = [ for i in 1..repeat_num -> i ]

    let is_divisible x =
        if (x % every_num) = 0 then
            new_bar
        else
            old_bar

    let bars_ast = (List.map (fun bar_num -> is_divisible bar_num) bar_nums)

    let bars_ps =
        List.map
            (fun expr ->
                TRANSLATE
                + LINE(MAX_LINE_WIDTH)
                + (evalManyDrumPatterns expr envPattern numBeats div))
            bars_ast

    (bars_ps |> String.concat "\n")

(*
  given a list of expressions in a snippet, the expressions are either:
    - list of bars
    - list of repeat instructions without changes
    - list of repeat instructions with changes, and
        - change option is a list of numbers of bars to change
        - change option is number N such that bars are changed every N-th bar
*)
let evalSnippet
    snippet
    (envPattern: Map<PatternName, Note list>)
    (envBar: Map<BarName, DrumPattern list>)
    numBeats
    div
    : string =
    let rec evalSnippetHelper snippet : string =
        match snippet with
        | [] -> ""
        // look at one expression
        | head :: tail ->
            match head with
            // if it is a repeat with no change expr
            | Repeat (repeat_num, bars) ->
                // printfn "Repeat: %A %A" head bars
                // and repeat number is valid
                if repeat_num > 0 then
                    // evaluate the bars in that repeat expr
                    let bar = evalManyBars bars envPattern envBar numBeats div

                    (String.replicate repeat_num bar)
                    + (evalSnippetHelper tail)
                else
                    failwith ("Can't have negative repeat values.")
            // repeat with change, for example:
            // (case 1) repeat 4 : barname (1,2, ...) { ... }         OR
            // (case 2) repeat 4 : barname (every 3) { ... }     OR
            // (case 3) repeat 4 : [ barname1 barname2 ... ]
            | RepeatChange (repeat_num, modify_bar, option, modify_data) ->
                // printfn "RepeatChange: %A %A %A" head modify_bar modify_data

                match option with
                // (case 1) if repeat option is (1,2, ...)
                | Literals (literals) ->
                    if repeat_num > 0 then
                        // if the bar to modify exists
                        if envBar.ContainsKey modify_bar then
                            let expr = envBar.Item modify_bar
                            // evaluate the "repeat with change given a list of literals" expression
                            (evalRepeatChange repeat_num literals expr modify_data numBeats div envPattern)
                            + (evalSnippetHelper tail)
                        else
                            failwith ("Undefined bar " + modify_bar + ".")
                    else
                        failwith ("Can't have negative repeat values.")
                | Every (every_num) ->
                    if repeat_num > 0 then
                        // if the bar to modify exists
                        if envBar.ContainsKey modify_bar then
                            let expr = envBar.Item modify_bar
                            // evaluate the "repeat with change given a list of literals" expression
                            (evalRepeatChangeEvery repeat_num every_num expr modify_data numBeats div envPattern)
                            + (evalSnippetHelper tail)
                        else
                            failwith ("Undefined bar " + modify_bar + ".")
                    else
                        failwith ("Can't have negative repeat values.")


    evalSnippetHelper snippet

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

    // printfn "%A, %A" title subtitle
    // printfn "%A" bars
    let header = TITLE(title) + SUBTITLE(subtitle)
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
        let drums = createPatternPS HH expr numBeats div

        // construct PostScript
        header
        + LINE(MAX_LINE_WIDTH)
        + TIME_SIG(numBeats, beatValue)
        + drums
    // render value is a bar
    elif envBar.ContainsKey render then
        let expr = envBar.Item render
        let drums = evalManyDrumPatterns expr envPattern numBeats div
        // printfn "%A" drums

        // construct PostScript
        header
        + LINE(MAX_LINE_WIDTH)
        + TIME_SIG(numBeats, beatValue)
        + drums
    // render value is a snippet
    elif envSnippet.ContainsKey render then
        let expr = envSnippet.Item render
        printfn "%A" expr
        let drums = evalSnippet expr envPattern envBar numBeats div

        // construct PostScript
        header
        + TIME_SIG(numBeats, beatValue)
        + FIRST_LINE
        + drums
    else
        failwith ("Undefined variable '" + render + "'")
