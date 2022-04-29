module ProjectParser

open FParsec

type Count =
    | Num of int
    | E of char
    | And of char
    | A of char

type Notes = Count list

// type Pattern = string * Notes list


// type Count =
//     | num of int
//     | e of char
//     | plus of char
//     | a of char

// type Notes = Count list

// type Pattern = Notes list

// pattern my_pattern = 1 2 3 4

// parse notes
// let num = pdigit |>> int |>> Num
// let e = pletter |>> E
// let and' = pletter |>> And
// let a = pletter |>> A

// let ws = spaces
let ws0 = spaces
let ws1 = spaces1

let str s = pstring s
let str_ws0 s = pstring s .>> ws0

type Pattern = Pattern of string * (string list)

let pattern_id = "pattern"

let valid_word: Parser<_, unit> =
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

let p_pattern: Parser<Pattern, Unit> =
    pipe3
        (str pattern_id .>> ws1)
        (manyCharsTill ((letter <|> digit) .>> ws0) (str_ws0 ":"))
        (many p_notes)
        (fun _ id data -> Pattern(id, data))
