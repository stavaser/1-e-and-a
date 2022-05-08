open ProjectInterpreter
open ProjectParser
open System.IO
open FParsec
open ProjectParser
open Giraffe.ViewEngine

[<EntryPoint>]
let main argv =
    if (Array.length argv <> 1) then
        printfn "usage: dotnet run <input_file>.1ea"
        exit 1

    let input = argv.[0]

    let test p str =
        match run p str with
        | Success (result, _, _) ->
            let output = eval result
            File.Copy("src.ps", "output.ps", true)
            // use sw = new File.AppendText("output.ps")
            use sw = new StreamWriter("output.ps", true)

            // File.AppendText(output)
            sw.WriteLine(output)
        // printfn "Success: %A" (output)
        | Failure (errorMsg, _, _) -> printfn "Failure: %s" errorMsg


    let lines = File.ReadAllText input
    test grammar lines

    0
