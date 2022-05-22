module lang.ProjectInterpreter

open lang.ProjectParser

exception RuntimeError of string

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


let combineBeatsAndRests beats rests =
    List.map2
        (fun notes rest ->
            let (value, order) = List.head notes

            (rest :: value, order))
        beats
        rests

let rec transpose list =
    if List.isEmpty (List.head list) then
        []
    else
        (List.map List.head list |> List.distinct)
        :: transpose (List.map List.tail list)


let beatSort beat =
    beat
    |> List.map (fun x ->
        x
        |> List.map (fun y ->
            match y with
            | Num (n), _ -> (y, 1)
            | E, _ -> (y, 2)
            | And, _ -> (y, 3)
            | A, _ -> (y, 4))

        |> List.sortBy (fun (data, index) -> index)
        |> List.map (fun (data, index) -> data))


let separatePattern drum pattern =
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

let evalBeats beats =
    let rec evalPatternHelper beat =
        match beat with
        | [] -> []
        | (head, drums) :: tail ->
            let next =
                if List.isEmpty tail then
                    head
                else
                    (List.head tail) |> (fun (note, _) -> note)

            ((beam head next), drums)
            :: (evalPatternHelper tail)
    List.map (fun x -> evalPatternHelper x) beats


let manyPatternsToString patterns =
    let drum_ABC drum =
        match drum with
        | CC -> "na"
        | RD -> "n^f"
        | HH -> "ng"
        | SN -> "c"
        | T1 -> "e"
        | T2 -> "B"
        | FT -> "A"
        | BD -> "F"

    patterns
    |> List.map (fun x ->
        x
        |> List.map (fun (value, drums) ->
            (drums
             |> List.map (fun x -> drum_ABC x + (string value)))
            |> String.concat "")
        |> List.map (fun x -> "[" + x + "]")
        |> String.concat "")
    |> String.concat " "


let evalBar bar _params (envPattern: Map<PatternName, Note list>) =
    let rec evalBarHelper bar =
        match bar with
        | [] -> []
        | head :: tail ->
            match head with
            | DrumPatternNotes (drum, notes) ->
                let separated = separatePattern drum notes
                separated :: (evalBarHelper tail)
            | DrumPatternVar (drum, var) ->
                if envPattern.ContainsKey var then
                    let notes = envPattern.Item var
                    let separated = separatePattern drum notes
                    separated :: (evalBarHelper tail)
                else
                    raise (RuntimeError("Undefined pattern " + var + "."))

    let bars = evalBarHelper bar

    let transformed =
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
        |> beatSort

    let evaluatedBeats = evalBeats transformed
    let string = manyPatternsToString evaluatedBeats + "|\n"
    // printfn "bars: %A" bars
    // printfn "transformed: %A" transformed
    // printfn "evaluatedBeats: %A" evaluatedBeats
    // // printfn "transposed: %A" transposed
    // // printfn "combined: %A" combined
    // printfn "string: %A" string
    string

let evalManyBars bars _params (envPattern: Map<PatternName, Note list>) (envBar: Map<BarName, DrumPattern list>) =
    bars
    |> List.map (fun bar ->
        if envBar.ContainsKey bar then
            let drum_pattern = envBar.Item bar
            evalBar drum_pattern _params envPattern
        else
            raise (RuntimeError("Undefined bar '" + bar + "'")))
    |> String.concat ""

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

let evalRepeatChange repeat_num literals old_bar change_data _params (envPattern: Map<PatternName, Note list>) =
    if repeat_num <= 0 then
        raise (RuntimeError("Can't have " + (string repeat_num) + " as a change value."))
    else
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
    printfn "\nevalBarDifference: %A \n" new_bar

    let newMap =
        Map.fold (fun acc key value -> Map.add key new_bar acc) all_old_bars all_new_bars

    let bars_ast = (List.map (fun (_, data) -> data) (newMap |> Map.toList))
    printfn "\evalBar: %A \n" (evalBar new_bar _params envPattern)

    bars_ast
    |> List.map (fun expr -> evalBar expr _params envPattern)
    |> String.concat ""


let evalRepeatChangeEvery repeat_num every_num old_bar change_data _params (envPattern: Map<PatternName, Note list>) =
    let new_bar = evalBarDifference old_bar change_data

    let bar_nums = [ for i in 1..repeat_num -> i ]

    let is_divisible x =
        if (x % every_num) = 0 then
            new_bar
        else
            old_bar

    let bars_ast = (List.map (fun bar_num -> is_divisible bar_num) bar_nums)

    bars_ast
    |> List.map (fun expr -> evalBar expr _params envPattern)
    |> String.concat ""


let evalSnippet expr _params (envPattern: Map<PatternName, Note list>) (envBar: Map<BarName, DrumPattern list>) =
    let rec evalSnippetHelper snippet =
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
                    let bar = evalManyBars bars _params envPattern envBar

                    (String.replicate repeat_num bar)
                    + (evalSnippetHelper tail)
                else
                    raise (RuntimeError("Can't have " + (string repeat_num) + " as a repeat value."))
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
                        if List.contains 0 literals then
                            raise (RuntimeError("Can't have 0 as a change value."))
                        else
                            if envBar.ContainsKey modify_bar then
                                let expr = envBar.Item modify_bar
                                // evaluate the "repeat with change given a list of literals" expression
                                (evalRepeatChange repeat_num literals expr modify_data _params envPattern)
                                + (evalSnippetHelper tail)
                            else
                                raise (RuntimeError("Undefined bar " + modify_bar + "."))
                    else
                        raise (RuntimeError("Can't have " + (string repeat_num) + " as a repeat value."))
                | Every (every_num) ->
                    if repeat_num > 0 then
                        if every_num = 0 then
                            raise (RuntimeError("Can't have 0 as a change value."))
                        else
                            // if the bar to modify exists
                            if envBar.ContainsKey modify_bar then
                                let expr = envBar.Item modify_bar
                                // evaluate the "repeat with change given a list of literals" expression
                                (evalRepeatChangeEvery repeat_num every_num expr modify_data _params envPattern)

                                + (evalSnippetHelper tail)
                            else
                                raise (RuntimeError("Undefined bar " + modify_bar + "."))
                    else
                        raise (RuntimeError("Can't have " + (string repeat_num) + " as a repeat value."))

    evalSnippetHelper expr

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
    let ((numBeats, beatValue), div, tempo, title) =
        (fun { Time = (a, b)
               Division = (_, y)
               Tempo = tempo
               Title = title
          } -> (int a, int b), float y, tempo, title)
            settings

    if numBeats <> 4 || beatValue <> 4 then
        raise (RuntimeError("Time: " + (string numBeats) + "/" + (string beatValue) + " feature isn't supported yet! Try the following: 4/4"))
    
    if div <> 16 then
        raise (RuntimeError("Division: 1/" + (string div) + " feature isn't supported yet! Try the following: 1/16"))
    
    let _params = (numBeats, beatValue, div)
    let header_settings = "Q: " + (string tempo) + "\nT: " + title + "\nM: " + (string numBeats) + "/" + (string beatValue) + "\nL: 1/" + (string div) + "\nV:ALL stem=up\n"

    let header =
        "%%percmap D  pedal-hi-hat x
%%percmap E  bass-drum-1
%%percmap F  acoustic-bass-drum
%%percmap G  low-floor-tom
%%percmap A  high-floor-tom
%%percmap B  low-tom
%%percmap ^B tambourine   triangle
%%percmap c  acoustic-snare
%%percmap _c electric-snare
%%percmap ^c low-wood-block   triangle
%%percmap =c side-stick x
%%percmap d  low-mid-tom
%%percmap ^d hi-wood-block    triangle
%%percmap e  hi-mid-tom
%%percmap ^e cowbell      triangle
%%percmap f  high-tom
%%percmap ^f ride-cymbal-1
%%percmap g  closed-hi-hat
%%percmap ^g open-hi-hat
%%percmap a  crash-cymbal-1  x
%%percmap ^a open-triangle     triangle
%%MIDI drummap ^g 42 %closed hi hat
%%MIDI drummap _g 46 % open hi hat
%%flatbeams 1
X: 1
U:n=!style=x!
K:perc\n" + header_settings

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

    let rec evalRender render_list = 
        match render_list with
        | [] -> ""
        | render::tail -> 
            // render value is a pattern
            if envPattern.ContainsKey render then
                raise (RuntimeError("Cannot render patterns. Consider putting pattern inside a bar."))
            // render value is a bar
            elif envBar.ContainsKey render then
                let expr = envBar.Item render
                printfn "%A" expr
                let result = evalBar expr _params envPattern
                result  + (evalRender tail)
            // render value is a snippet
            elif envSnippet.ContainsKey render then
                let expr = envSnippet.Item render
                let result = evalSnippet expr _params envPattern envBar
                printfn "%A" result
                result + (evalRender tail)
            else
                raise (RuntimeError("Undefined variable '" + render + "'"))
    
    header + (evalRender render)