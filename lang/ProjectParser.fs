module ProjectParser

open FParsec

type Settings =
    { Time: uint8 * uint8
      Division: uint8 * uint8 }

type Note =
    | Num of uint8
    | E
    | And
    | A
    | Sep

type Drum =
    | HH
    | SN
    | BD

type PatternName = string
type BarName = string

type DrumPatternVar = Drum * PatternName
type DrumPatternNotes = Drum * (Note list)

type Pattern = Pattern of PatternName * (Note list)
type Bar = Bar of BarName * (DrumPatternVar list * DrumPatternNotes list)

type Expr =
    { Settings: Settings
      Patterns: Pattern list
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
let hh: Parser<Drum, unit> = (str_ws0 "hh") |>> (fun x -> HH)
let sn: Parser<Drum, unit> = (str_ws0 "sn") |>> (fun x -> SN)
let bd: Parser<Drum, unit> = (str_ws0 "bd") |>> (fun x -> BD)


(*
    keywords
*)
let pattern_keyword = "pattern"
let bar_keyword = "bar"
let time_keyword = "time"
let div_keyword = "division"
let tempo_keyword = "tempo"
let render_keyword = "render"


(*
    Parses which expression the program should render, such as
    render: beat2
*)
let p_render =
    ((str_ws0 render_keyword) .>> (ws0 >>. str_ws0 ":"))
    >>. (manyCharsTill (letter <|> digit) ws1)


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
let p_drum = (hh <|> sn <|> bd) .>> (ws0 >>. str_ws0 ":")

(*
    Parses a drum to pattern or pattern variable assignment such as
    hh: 1 e and a
    or
    hh: var_pattern_name1
*)
let p_drumpattern_notes =
    pipe2 p_drum (many p_note) (fun drum pattern -> DrumPatternNotes(drum, pattern))

let p_drumpattern_var =
    pipe2 p_drum (manyCharsTill (letter <|> digit) ws1) (fun drum pattern -> DrumPatternVar(drum, pattern))


(*
    Parses a bar expression such as
    bar mybar:
        hh: mypattern
        sn: 1 2 3 e 4
*)
// let p_bar: Parser<Bar, Unit> =
//     pipe3
//         (str_ws1 bar_keyword)
//         (p_assignment |>> BarName)
//         ((many p_drumpattern_var
//           .>>.? many p_drumpattern_notes))
//         (fun _ id data -> Bar(id, data))



let expr =
    pipe3 p_settings (many p_pattern) p_render (fun settings patterns render ->
        { Settings = settings
          Patterns = patterns
          Render = render })

// let expr = p_settings .>>. (many p_pattern) .>> spaces

let grammar: Parser<_, _> = expr .>> eof
