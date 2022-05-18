namespace ParserTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open ProjectParser
open System.IO
open ProjectInterpreter
open FParsec

[<TestClass>]
type TestClass() =

    [<TestMethod>]
    member this.TestMethodPassing() = Assert.IsTrue(true)

    [<TestMethod>]
    member this.TestBar() =
        let lines =
            "time :  4/4
            division: 1/16
            title: helo
            subtitle: new

            pattern mypattern : 1 e + a| 2 | 3 | 4 |

            bar mybar1:
                cc: [ 1 + | 2  + | 3 + | 4  + |  ]
                hh: [ 1 + | 2  + | 3 + | 4  + | ]
                sn: [ mypattern ]
                bd: [ 1  | 2   | 3  | 4 e + a  | ]

            render: mybar1
            "

        let test p str =
            match run p str with
            | Success (result, _, _) -> eval result
            | Failure (errorMsg, _, _) -> "error"

        let actual = (test grammar lines)

        let expected =
            "340.8 4.0 M  20.0 F2 (helo)showc
    0 -19.00 T 
340.8 3.2 M  16.0 F2 (new)showc
    0 -12.00 T
681.6 newline
(4)(4) 30.5 -47.0 tsig
60 crash

137.7 crash

215.4 crash

293.1 crash

370.8 crash

448.5 crash

526.2 crash

603.9000000000001 crash

60 closedhh

137.7 closedhh

215.4 closedhh

293.1 closedhh

370.8 closedhh

448.5 closedhh

526.2 closedhh

603.9000000000001 closedhh

60 snare

98.85 snare

137.7 snare

176.54999999999998 snare

215.4 snare

370.8 snare

526.2 snare

60 bass

215.4 bass

370.8 bass

526.2 bass

565.0500000000001 bass

603.9000000000001 bass

642.7500000000001 bass
"

        // My evaluated output is a file. I couldn't figure out how to open a file in Test.fs
        // this method I am trying to do doesn't work.
        Assert.AreEqual(expected, actual)
