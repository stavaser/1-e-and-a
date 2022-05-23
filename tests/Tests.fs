namespace ParserTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open lang.ProjectParser
open System.IO
open lang.ProjectInterpreter
open lang.ProjectHandler
open FParsec

[<TestClass>]
type TestClass() =

    (* Tests if the parser correctly parses a bar expression into AST *)
    [<TestMethod>]
    member this.TestParseBar() =
        let lines =
            "bar mybar:
  hh: [ sixteenth_notes ]
  sn: [   | 2 |   | 4 |]
  bd: [ 1 |   | 3 |   |]"

        let actual = 
            match run p_bar lines with
            | Success (result, _, _) -> result |> string
            | Failure (error, _, _) -> error

        let expected = ("mybar",
                [DrumPatternVar (HH, "sixteenth_notes");
                DrumPatternNotes (SN, [Sep; Num 2uy; Sep; Sep; Num 4uy; Sep]);
                DrumPatternNotes (BD, [Num 1uy; Sep; Sep; Num 3uy; Sep; Sep])]) |> Bar |> string

        Assert.AreEqual(expected, actual)




    (* Tests if the interpreter correctly evaluates a list of beats into
        a formatted version ready for mapping into strings.
     *)
    [<TestMethod>]
    member this.TestEvalBeats() =
        let beats = [[(Num 1uy, [CC; SN; BD]); (E, [SN]); (And, [SN]); (A, [SN])];
                    [(Num 2uy, [CC; T1]); (E, [T1]); (And, [T1]); (A, [T1])];
                    [(Num 3uy, [CC; SN; BD]); (E, [SN]); (And, [SN]); (A, [SN])];
                    [(Num 4uy, [CC; T2]); (E, [T2]); (And, [T2]); (A, [T2])]]
        let actual = 
            let output =
                try
                    evalBeats beats |> string
                with
                | RuntimeError (message) -> message
            output

        let expected = [[(1, [CC; SN; BD]); (1, [SN]); (1, [SN]); (1, [SN])];
                        [(1, [CC; T1]); (1, [T1]); (1, [T1]); (1, [T1])];
                        [(1, [CC; SN; BD]); (1, [SN]); (1, [SN]); (1, [SN])];
                        [(1, [CC; T2]); (1, [T2]); (1, [T2]); (1, [T2])]] |> string
                        
        Assert.AreEqual(expected, actual)
