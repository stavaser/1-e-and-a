open ProjectInterpreter
open ProjectParser
open System.IO
open FParsec
open ProjectParser


[<EntryPoint>]
let main argv =
    // let lines = File.ReadAllLines(@"./test.1ea")

    // // Convert file lines into a list.
    // let list = Seq.toList lines
    // printfn "%A" list

    // let output = Seq.where (fun (n) -> pattern_name n) list
    // printfn "%A" output
    // 0

    let input = argv.[0]

    let test p str =
        match run p str with
        | Success (result, _, _) -> printfn "Success: %A" result
        | Failure (errorMsg, _, _) -> printfn "Failure: %s" errorMsg

    test p_pattern input

    0
