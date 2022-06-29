module lang.ProjectParser

open FParsec

type Settings =
    { Time: uint8 * uint8
      Tempo: uint8
      Title: string }

type Note =
    | Num of uint8
    | Filler1
    | Filler2
    | Filler3
    | Filler4
    | E
    | And
    | A
    | Sep
    | Empty

type Drum =
    | CC
    | RD
    | HH
    | SN
    | T1
    | T2
    | FT
    | BD
    | Rest

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

type RepeatOption =
    | Literals of (int list)
    | Every of int

type SnippetData =
    | SnippetBar of BarName
    | Repeat of int * (BarName list)
    | RepeatChange of int * BarName * RepeatOption * (DrumPattern list)

type Snippet = Snippet of SnippetName * (SnippetData list)

type Expr =
    { Settings: Settings
      Patterns: Pattern list
      Bars: Bar list
      Snippets: Snippet list
      Render: string list }

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
let filler1: Parser<Note, unit> = (between (str_ws0 "(") (str_ws0 ")") (pchar '1' .>> ws0)) |>> (fun e -> Filler1)
let filler2: Parser<Note, unit> = (between (str_ws0 "(") (str_ws0 ")") (pchar '2' .>> ws0)) |>> (fun e -> Filler2)
let filler3: Parser<Note, unit> = (between (str_ws0 "(") (str_ws0 ")") (pchar '3' .>> ws0)) |>> (fun e -> Filler3)
let filler4: Parser<Note, unit> = (between (str_ws0 "(") (str_ws0 ")") (pchar '4' .>> ws0)) |>> (fun e -> Filler4)

(*
    Parse drum names
*)
let cc: Parser<Drum, unit> = (str_ws0 "cc") |>> (fun x -> CC)
let rd: Parser<Drum, unit> = (str_ws0 "rd") |>> (fun x -> RD)
let hh: Parser<Drum, unit> = (str_ws0 "hh") |>> (fun x -> HH)
let sn: Parser<Drum, unit> = (str_ws0 "sn") |>> (fun x -> SN)
let t1: Parser<Drum, unit> = (str_ws0 "t1") |>> (fun x -> T1)
let t2: Parser<Drum, unit> = (str_ws0 "t2") |>> (fun x -> T2)
let ft: Parser<Drum, unit> = (str_ws0 "ft") |>> (fun x -> FT)
let bd: Parser<Drum, unit> = (str_ws0 "bd") |>> (fun x -> BD)
let rest: Parser<Drum, unit> = (str_ws0 "r") |>> (fun x -> Rest)


(*
    keywords
*)
let pattern_keyword = "pattern"
let bar_keyword = "bar"
let snippet_keyword = "snippet"
let repeat_keyword = "repeat"
let change_repeat_keyword = "change"
let time_keyword = "time"
let div_keyword = "division"
let tempo_keyword = "tempo"
let render_keyword = "render"
let title_keyword = "title"
let subtitle_keyword = "subtitle"


(*
    Debug parser
*)
let (<!>) (p: Parser<_, _>) label : Parser<_, _> =
    fun stream ->
        // printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        // printfn "%A: Leaving %s (%A) %A" stream.Position label reply.Status reply.Result
        // printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply

let variable_name:Parser<string, unit> = (manyChars (letter <|> pchar '_'))

(*
    Parses which expression the program should render, such as
    render: beat2
*)
let p_render =
    (((str_ws0 render_keyword) .>> (ws0 >>. str_ws0 ":"))
     >>. (str_ws0 "{" >>. sepBy (variable_name .>> ws0) (str_ws0 ",") .>> str_ws0 "}"))

(*
    Parses settings
*)
let p_settings =
    let p_time: Parser<(uint8 * uint8), unit> =
        ((str_ws0 time_keyword) .>> (ws0 >>. str_ws0 ":"))
        >>. (((puint8 .>> ws0) .>> pchar '/')
             .>>. (puint8 .>> ws0))

    let p_tempo: Parser<(uint8), unit> =
        ((str_ws0 tempo_keyword) .>> (ws0 >>. str_ws0 ":"))
        >>. (puint8 .>> ws0)

    let p_string =
        (many1CharsTill anyChar newline)
        <!> "parsing a string"

    pipe3
        p_time
        p_tempo
        (((((str_ws0 title_keyword) .>> (ws0 >>. str_ws0 ":"))
           >>. p_string)
          .>> ws0)
         <!> "parsed title")
        (fun time tempo title ->
            { Time = time
              Tempo = tempo
              Title = title
              })

(*
    Parses a variable name assignment such as
    var_pattern_name1:
*)
let p_assignment = (variable_name .>> (ws0 >>. str_ws0 ":"))

(*
    Parses a note value such as
    1, e, and, or a
*)
let p_note = (num <|> e <|> and' <|> a <|> sep <|> attempt filler1 <|>  attempt filler2 <|> attempt filler3 <|> attempt filler4)

(*
    Parses a pattern expression such as
    pattern var_pattern_name1: 1 2 and a
*)
let p_pattern: Parser<Pattern, Unit> =
    pipe3 (str_ws1 pattern_keyword) (p_assignment |>> PatternName) (between (str_ws0 "[") (str_ws0 "]") (many p_note)) (fun _ id data -> Pattern(id, data))

(*
    Parses drum name assignment such as
    hh:
*)
let p_drum =
    ((cc <|> rd <|> hh <|> sn <|> t1 <|> t2 <|> bd <|> ft <|> rest)
     <!> "parsing drums")
    .>> ((ws0 <!> "parsing whitespace")
         >>. (str_ws0 ":" <!> "parsing colon"))

(*
    Parses a drum to pattern assignment
*)
let p_drumpattern_notes =
    pipe2
        p_drum
        ((str_ws0 "[" >>. (many p_note .>> ws0)
          .>> str_ws0 "]")
         <!> "drum->notes")
        (fun drum pattern -> DrumPatternNotes(drum, pattern))

(*
    Parses a drum to pattern variable assignment
*)
let p_drumpattern_var =
    pipe2
        p_drum
        ((str_ws0 "[") >>. (variable_name .>> (ws0 >>. str_ws0 "]")))
        (fun drum pattern -> DrumPatternVar(drum, pattern))

(*
    Parses a bar expression such as
    bar mybar:
        hh: [mypattern]
        sn: [1 |2| 3| e |4|]
*)
let p_bar: Parser<Bar, Unit> =
    pipe3
        (str_ws1 bar_keyword)
        (p_assignment |>> BarName)
        ((many (
            attempt p_drumpattern_notes
            <|> attempt p_drumpattern_var
         ))
         <!> "trying to parse drum->notes or drum->var")
        (fun _ id data -> Bar(id, data))

(*
    Parses a snippet
*)
let p_snippet: Parser<Snippet, Unit> =
    let number = many1Satisfy isDigit

    let p_barname = (variable_name) |>> BarName

    let p_repeat_num =
        ((many1Satisfy isDigit) .>> (ws0 >>. str_ws0 ":"))
        |>> int

    let p_change_num =
        (str_ws0 "(" >>. sepBy ((number .>> ws0) |>> int) (str_ws0 ",") .>> str_ws0 ")")
        |>> Literals

    let p_change_every =
        (str_ws0 "("
         >>. (str_ws0 "every" >>. ((number .>> ws0) |>> int))
         .>> str_ws0 ")")
        |>> Every

    let p_change_data =
        str_ws0 "{" >>. (many (
            attempt p_drumpattern_notes
            <|> attempt p_drumpattern_var
         ))
        .>> str_ws0 "}"

    let p_repeat =
        pipe3
            (str_ws1 repeat_keyword)
            (p_repeat_num)
            (str_ws0 "{" >>. sepBy (p_barname .>> ws0) (str_ws0 ",") .>> str_ws0 "}")
            (fun _ repeat_num barname -> Repeat(repeat_num, barname))

    let p_repeat_with_change =
        pipe5
            (str_ws1 change_repeat_keyword)
            (p_repeat_num)
            (p_barname .>> ws0)
            (attempt p_change_num <|> attempt p_change_every)
            (p_change_data)
            (fun _ repeat_num barname change_num change_data ->
                RepeatChange(repeat_num, barname, change_num, change_data))

    pipe3
        (str_ws1 snippet_keyword)
        (p_assignment |>> SnippetName)
        (many (
            (attempt p_repeat_with_change)
            <|> (attempt p_repeat)
        ))
        (fun _ id data -> Snippet(id, data))


let expr =
    pipe5
        (p_settings .>> ws0)
        (many (p_pattern .>> ws0))
        (many p_bar .>> ws0)
        (many p_snippet .>> ws0)
        (p_render .>> ws0)
        (fun settings patterns bars snippets render ->
            { Settings = settings
              Patterns = patterns
              Bars = bars
              Snippets = snippets
              Render = render })

let grammar: Parser<_, _> = expr .>> eof
