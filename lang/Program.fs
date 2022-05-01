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
        1
    else
        let input = argv.[0]

        let test p str =
            match run p str with
            | Success (result, _, _) -> printfn "Success: %A" (eval result)
            | Failure (errorMsg, _, _) -> printfn "Failure: %s" errorMsg


        let lines = File.ReadAllText input
        // let list = Seq.toList lines
        test grammar lines

        // List.map (fun x -> test parse x) list |> ignore

        // let output = Seq.where (fun (n) -> test p_pattern n) list
        // printfn "%A" output
        // test p_pattern input

        0
