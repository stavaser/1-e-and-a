module ProjectParser

open FParsec

type Settings =
    { Time: uint8 * uint8
      Division: uint8 * uint8
    //   Title: string
    //   Subtitle: string

     }

type Note =
    | Num of uint8
    | E
    | And
    | A
    | Sep

type Drum =
    | CC
    | RD
    | HH
    | SN
    | T1
    | T2
    | BD

type PatternName = string
type BarName = string
type SnippetName = string

type DrumPattern =
    | DrumPatternVar of Drum * PatternName
    | DrumPatternNotes of Drum * (Note list)

// type DrumPatternVar = Drum * PatternName
// type DrumPatternNotes = Drum * (Note list)

type Pattern = Pattern of PatternName * (Note list)
type Bar = Bar of BarName * (DrumPattern list)


// type DrumPattern =
//     | DrumPatternVar of Drum * PatternName
//     | DrumPatternNotes of Drum * (Note list)

// type DrumPatternVar = Drum * PatternName
// type DrumPatternNotes = Drum * (Note list)

// type Pattern = Pattern of PatternName * (Note list)
// type Bar = Bar of BarName * (DrumPatternVar list * DrumPatternNotes list)

type SnippetData =
    | SnippetBar of BarName
    | Repeat of int * (BarName list)
    | RepeatChange of int * BarName * int * (DrumPattern list)

type Snippet = Snippet of SnippetName * (SnippetData list)

type Expr =
    { Settings: Settings
      Patterns: Pattern list
      Bars: Bar list
      Snippets: Snippet list
      Render: string }

let ws0 = spaces
let ws1 = spaces1

let str s = pstring s
let str_ws0 s = pstring s .>> ws0
let str_ws1 s = pstring s .>> ws1

(*
    Parse note values
*)
let num: Parser<Note, unit> = (puint8 .>> ws0) |>> (fun e -> Num(e))
let e: Parser<Note, unit> = (pchar 'e' .>> ws0) |>> (fun e -> E)
let and': Parser<Note, unit> = (pchar '+' .>> ws0) |>> (fun e -> And)
let a: Parser<Note, unit> = (pchar 'a' .>> ws0) |>> (fun e -> A)
let sep: Parser<Note, unit> = (pchar '|' .>> ws0) |>> (fun e -> Sep)

(*
    Parse drum names
*)
let cc: Parser<Drum, unit> = (str_ws0 "cc") |>> (fun x -> CC)
let rd: Parser<Drum, unit> = (str_ws0 "rd") |>> (fun x -> RD)
let hh: Parser<Drum, unit> = (str_ws0 "hh") |>> (fun x -> HH)
let sn: Parser<Drum, unit> = (str_ws0 "sn") |>> (fun x -> SN)
let t1: Parser<Drum, unit> = (str_ws0 "t1") |>> (fun x -> T1)
let t2: Parser<Drum, unit> = (str_ws0 "t2") |>> (fun x -> T2)
let bd: Parser<Drum, unit> = (str_ws0 "bd") |>> (fun x -> BD)


(*
    keywords
*)
let pattern_keyword = "pattern"
let bar_keyword = "bar"
let snippet_keyword = "pippet"
let repeat_keyword = "repeat"
let time_keyword = "time"
let div_keyword = "division"
let tempo_keyword = "tempo"
let render_keyword = "render"
let title_keyword = "title"
let subtitle_keyword = "subtitle"


(*
    Parses which expression the program should render, such as
    render: beat2
*)
let p_render =
    (((str_ws0 render_keyword) .>> (ws0 >>. str_ws0 ":"))
     >>. (manyCharsTill (letter <|> digit) ws1))

(*
    Parses the setup such as
    time: 4/4
    division: 1/16
*)
let p_settings =
    let p_time: Parser<(uint8 * uint8), unit> =
        ((str_ws0 time_keyword) .>> (ws0 >>. str_ws0 ":"))
        >>. (((puint8 .>> ws0) .>> pchar '/')
             .>>. (puint8 .>> ws0))

    let p_div: Parser<(uint8 * uint8), unit> =
        ((str_ws0 div_keyword) .>> (ws0 >>. str_ws0 ":"))
        >>. (((puint8 .>> ws0) .>> pchar '/')
             .>>. (puint8 .>> ws0))

    // let p_subtitle =
    //     ((str_ws0 subtitle_keyword)
    //      .>> (ws0 >>. str_ws0 ":"))
    //     >>. (manyCharsTill (letter <|> digit) (ws0 >>. tab))

    // let p_title =
    //     ((str_ws0 title_keyword) .>> (ws0 >>. str_ws0 ":"))
    //     >>. (manyCharsTill (letter <|> digit) (ws0 >>. tab))

    // pipe4 p_time p_div p_title p_subtitle (fun time div title subtitle ->
    //     { Time = time
    //       Division = div
    //       Title = title
    //       Subtitle = subtitle })
    pipe2 p_time p_div (fun time div -> { Time = time; Division = div })

(*
    Parses a variable name assignment such as
    var_pattern_name1:
*)
let p_assignment = (manyCharsTill (letter <|> digit) (ws0 >>. str_ws0 ":"))

(*
    Parses a note value such as
    1, e, and, or a
*)
let p_note = (num <|> e <|> and' <|> a <|> sep)

(*
    Parses a pattern expression such as
    pattern var_pattern_name1: 1 2 and a
*)
let p_pattern: Parser<Pattern, Unit> =
    pipe3 (str_ws1 pattern_keyword) (p_assignment |>> PatternName) (many p_note) (fun _ id data -> Pattern(id, data))

(*
    Parses drum name assignment such as
    hh:
*)
let p_drum =
    (cc <|> rd <|> hh <|> sn <|> t1 <|> t2 <|> bd)
    .>> (ws0 >>. str_ws0 ":")

(*
    Parses a drum to pattern or pattern variable assignment such as
    hh: 1 e and a
    or
    hh: var_pattern_name1
*)
let p_drumpattern_notes =
    pipe2 p_drum (many p_note .>> ws0) (fun drum pattern -> DrumPatternNotes(drum, pattern))

let p_drumpattern_var =
    pipe2 p_drum (manyCharsTill (letter <|> digit) ws1) (fun drum pattern -> DrumPatternVar(drum, pattern))


(*
    Parses a bar expression such as
    bar mybar:
        hh: mypattern
        sn: 1 2 3 e 4
*)
let p_bar: Parser<Bar, Unit> =
    pipe3
        (str_ws1 bar_keyword)
        (p_assignment |>> BarName)
        (many (p_drumpattern_notes <|> p_drumpattern_var))
        (fun _ id data -> Bar(id, data))

let p_snippet: Parser<Snippet, Unit> =
    let number = many1Satisfy isDigit

    let p_barname = (manyCharsTill (letter <|> digit) ws1) |>> BarName

    let p_repeat_num =
        ((many1Satisfy isDigit) .>> (ws0 >>. str_ws0 ":"))
        |>> int

    let p_change_num = (str_ws0 "(" >>. number .>> str_ws0 ")") |>> int

    let p_change_data =
        str_ws0 "{" >>. (many p_drumpattern_notes)
        .>> str_ws0 "}"

    let p_repeat =
        pipe3
            (str_ws1 repeat_keyword)
            (p_repeat_num)
            (str_ws1 "[" >>. many p_barname .>> str_ws1 "]")
            (fun _ repeat_num barname -> Repeat(repeat_num, barname))

    let p_repeat_with_change =
        pipe5
            (str_ws1 repeat_keyword)
            (p_repeat_num)
            (p_barname)
            (p_change_num)
            (p_change_data)
            (fun _ repeat_num barname change_num change_data ->
                RepeatChange(repeat_num, barname, change_num, change_data))

    pipe3
        (str_ws1 snippet_keyword)
        (p_assignment |>> SnippetName)
        (attempt (many p_repeat_with_change)
         <|> (many p_repeat))
        (fun _ id data -> Snippet(id, data))


let expr =
    pipe5
        (p_settings)
        (many (p_pattern .>> ws0))
        (many p_bar .>> ws0)
        (many p_snippet .>> ws0)
        p_render
        (fun settings patterns bars snippets render ->
            { Settings = settings
              Patterns = patterns
              Bars = bars
              Snippets = snippets
              Render = render })

// let expr = p_settings .>>. (many p_pattern) .>> spaces

let grammar: Parser<_, _> = expr .>> eof
