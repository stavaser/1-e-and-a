open lang.ProjectHandler
open lang.ProjectParser

open System.IO

[<EntryPoint>]
let main argv =
    if (Array.length argv <> 1) then
        printfn "usage: dotnet run <input_file>"
        printfn "you can run an example file by typing: dotnet run example-3.1ea"
        printfn "the output of this is a text file that contains abc notaion code."
        printfn "Also check out the web editor! Read more in the docs: https://stump-pullover-0b3.notion.site/Docs-1-e-a-f458fb361a92429eb9122401cc244358"
        exit 1

    let input = argv.[0]

    let lines = File.ReadAllText input

    parseAndEval lines

    0
