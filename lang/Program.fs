open ProjectInterpreter
open ProjectParser
open System.IO
open FParsec
open ProjectParser
open Giraffe.ViewEngine

[<EntryPoint>]
let main argv =
    if (Array.length argv <> 1) then
        printfn "usage: dotnet run <input_file>"
        exit 1

    let input = argv.[0]

    let test p str =
        match run p str with
        | Success (result, _, _) ->
            let output = eval result
            use sw = new StreamWriter("output.html")
            sw.WriteLine(output)
            printfn "Success: %A" (eval result)
        | Failure (errorMsg, _, _) -> printfn "Failure: %s" errorMsg


    let lines = File.ReadAllText input
    test grammar lines

    0
