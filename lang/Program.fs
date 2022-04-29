open ProjectInterpreter
open ProjectParser
open System.IO
open FParsec
open ProjectParser
open Giraffe.ViewEngine




[<EntryPoint>]
let main argv =
    // let lines = File.ReadAllLines(@"./test.1ea")

    // // Convert file lines into a list.
    // let list = Seq.toList lines
    // printfn "%A" list

    // let output = Seq.where (fun (n) -> pattern_name n) list
    // printfn "%A" output
    // 0

    // let view =
    //     html [] [
    //         head [] [ title [] [ str "Giraffe" ] ]
    //         body [] [ header [] [ str "Giraffe" ] ]
    //     ]

    // let document = RenderView.AsString.htmlDocument view

    // printfn "%s" document

    let input = argv.[0]

    let test p str =
        match run p str with
        | Success (result, _, _) -> printfn "Success: %A" result
        | Failure (errorMsg, _, _) -> printfn "Failure: %s" errorMsg


    let lines = File.ReadAllText input
    // let list = Seq.toList lines
    test grammar lines

    // List.map (fun x -> test parse x) list |> ignore

    // let output = Seq.where (fun (n) -> test p_pattern n) list
    // printfn "%A" output
    // test p_pattern input

    0
