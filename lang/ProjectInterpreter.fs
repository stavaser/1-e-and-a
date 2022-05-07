module ProjectInterpreter

open System

open ProjectParser

let MAX_LINE_WIDTH = 681.6

let TIME_SIG (a, b) =
    "("
    + string a
    + ")("
    + string b
    + ") 30.5 -47.0 tsig\n"

let LINE width = string width + " newline\n"

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

// [100.0; 112.5; 125.0; 137.5]
let evalOneBeat beat numBeats div i =
    let rec evalOneBeatHelper (pattern: Note list) (numBeats: int) div distance prevVal =
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
    // where the first note of a beat should start
    let offset = 60.0
    let note_start = (MAX_LINE_WIDTH - offset) / float numBeats
    // minimum distance between two consequtive notes
    let dist = note_start / float numBeats

    List.mapi
        (fun i x -> evalOneBeat x numBeats dist ((float i) * note_start + offset))

        separatedPattern

let rec evalBeams distances offset =

    let rec evalBeamsHelper one_beat prev_note =
        match one_beat with
        | [] -> ""
        | note :: note_rest ->
            let len = note

            BEAM8(note, len)
            + (evalBeamsHelper note_rest note)

    match distances with
    | [] -> ""
    | one_beat :: rest ->
        (evalBeamsHelper one_beat 0.0)
        + evalBeams rest (offset + 1.0)

let createPatternPS drum pattern numBeats div =
    // 2d list of distances of notes in each beat
    let distances = evalOnePattern pattern numBeats div
    let beams = evalBeams distances 0
    printfn "%A" (distances)
    printfn "%A" (beams)

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
    + beams



let evalOneDrumPattern drum_pattern numBeats div =
    match drum_pattern with
    | DrumPatternNotes (drum, notes) -> createPatternPS drum notes numBeats div

let evanManyDrumPatterns drum_patterns numBeats div =
    List.map (fun expr -> (evalOneDrumPattern expr numBeats div)) drum_patterns
    |> String.concat "\n"

let eval
    { Settings = settings
      Patterns = patterns
      Bars = bars
      Snippets = snippets
      Render = render }
    =
    // get values of the settings
    let ((numBeats, beatValue), div) =
        (fun { Time = (a, b); Division = (_, y) } -> (int a, int b), float y) settings

    let envPattern =
        patterns
        |> List.map (fun (Pattern (id, data)) -> id, data)
        |> Map.ofSeq

    let envBar =
        bars
        |> List.map (fun (Bar (id, data)) -> id, data)
        |> Map.ofSeq

    let envSnippet =
        snippets
        |> List.map (fun (Snippet (id, data)) -> id, data)
        |> Map.ofSeq

    if envPattern.ContainsKey render then
        let expr = envPattern.Item render
        let drum = HH
        let drums = createPatternPS drum expr numBeats div

        LINE(MAX_LINE_WIDTH)
        + TIME_SIG(numBeats, beatValue)
        + drums
    elif envBar.ContainsKey render then
        let expr = envBar.Item render
        let drums = evanManyDrumPatterns expr numBeats div

        LINE(MAX_LINE_WIDTH)
        + TIME_SIG(numBeats, beatValue)
        + drums
    // evalOneBeat pattern numBeats div 100.0 (Num(uint8 1))
    elif envSnippet.ContainsKey render then
        let expr = envSnippet.Item render
        printfn "%A" expr
        "hehe"
    else
        failwith ("Undefined variable '" + render + "'")

// evalManyPatterns patterns envPattern render
