module lang.ProjectHandler

open lang.ProjectInterpreter
open lang.ProjectParser

open System.IO
open FParsec

let parseAndEval str =
    let parsed_str = run grammar str

    match parsed_str with
    | Success (result, _, _) ->
        let output =
            try
                eval result
            with
            | RuntimeError (message) -> message
        use sw = new StreamWriter("output.txt")
        sw.WriteLine(output)
        printfn "All done! Check the output in the file output.txt"
        printfn "Also check out the web editor! Read more in the docs: https://stump-pullover-0b3.notion.site/Docs-1-e-a-f458fb361a92429eb9122401cc244358"
        output
    | Failure (errorMsg, _, _) -> errorMsg

let testParser p str =
    match run p str with
    | Success (result, _, _) -> result
    | Failure (errorMsg, _, _) -> errorMsg
