open lang.ProjectHandler
open lang.ProjectParser

open System.IO

[<EntryPoint>]
let main argv =
    if (Array.length argv <> 1) then
        printfn "usage: dotnet run <input_file>.1ea"
        exit 1

    let input = argv.[0]

    let lines = File.ReadAllText input

    let res = parseAndEval lines
    res
    printfn "%A" res

    0
