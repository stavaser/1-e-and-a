module ProjectParser

open FParsec

type Note =
    | Num of int
    | E of char
    | And of char
    | A of char

type VarName = string

type Pattern = Pattern of VarName * (string list)


type Drum =
    | HH of string
    | SN of string
    | BD of string

type Bar = Bar of VarName * ((Drum * Pattern) list)
// type Expr =
// parse notes
// let num = pdigit |>> int |>> Num
// let e = pletter |>> E
// let and' = pletter |>> And
// let a = pletter |>> A

// type Expr =
//     | Pattern of VarName * (string list)
//     | Bar of VarName * ((Drum * Pattern) list)


let ws0 = spaces
let ws1 = spaces1

let str s = pstring s
let str_ws0 s = pstring s .>> ws0

let pattern_keyword = "pattern"
let bar_keyword = "bar"

let p_valid_word: Parser<_, unit> =
    many1Satisfy (fun c ->
        c <> '\\'
        && c <> '"'
        && c <> ' '
        && c <> '\n'
        && c <> '\r'
        && c <> '\t')

let p_notes: Parser<_, unit> =
    many1Satisfy (fun c ->
        isDigit c
        || c = 'e'
        || c = '+'
        || c = 'a'
        || c = '|')
    .>> ws0

let p_assignment = (manyCharsTill (letter <|> digit) (ws0 >>. str_ws0 ":"))

// "forward declare" <expr>
// let expr, exprImpl = createParserForwardedToRef ()

let p_pattern: Parser<Pattern, Unit> =
    pipe3 (str pattern_keyword .>> ws1) (p_assignment) (many (p_notes)) (fun _ id data -> Pattern(id, data))

// let p_pattern: Parser<Pattern, Unit> =
//     pipe3 (str pattern_keyword .>> ws1) (p_assignment) (many (p_notes)) (fun _ id data -> Pattern(id, data))

let p_drumpattern =
    pipe2 p_assignment p_pattern (fun drum pattern -> (HH(drum), pattern))

// let p_bar =
//     (((str bar_keyword .>> ws1) >>. (p_assignment))
//      .>>. (many p_drumpattern))
//     |>> Bar

let p_bar: Parser<Bar, Unit> =
    pipe3 (str bar_keyword .>> ws1) (p_assignment) (many p_drumpattern) (fun _ id data -> Bar(id, data))

let expr = (many p_pattern .>>. many p_bar) .>> spaces

let grammar: Parser<_, _> = expr .>> eof
