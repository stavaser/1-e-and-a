module lang.ProjectHandler

open lang.ProjectInterpreter
open lang.ProjectParser

open System.IO
open FParsec

let parseAndEval str =
    let parsed_str = run grammar str

    match parsed_str with
    | Success (result, _, _) ->
        let output = eval result
        use sw = new StreamWriter("output.txt")
        printfn "Success: %A" (output)
        sw.WriteLine(output)
        output
    | Failure (errorMsg, _, _) -> errorMsg

let testParser p str =
    match run p str with
    | Success (result, _, _) ->
        let output = eval result
        use sw = new StreamWriter("output.txt")
        printfn "Success: %A" (output)
        sw.WriteLine(output)
        output
    | Failure (errorMsg, _, _) -> errorMsg
