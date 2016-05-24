//   Copyright 2013, 2014 YaccConstructor Software Foundation
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.


module GLLAbstractParserTestsMain

open System.IO
open GLLAbstractParserTestsBio
open GLLAbstractParserTests
    [<Test>]
    member this._51_Conj1_Success () =
        let qGraph = new ParserInputGraph<_>(0, 10)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.Conj1.A 1)
             edg 1 2 (GLL.Conj1.A 2)
             edg 2 3 (GLL.Conj1.A 3)
             edg 3 4 (GLL.Conj1.B 3)
             edg 4 5 (GLL.Conj1.B 3)
             edg 5 6 (GLL.Conj1.B 3)
             edg 6 7 (GLL.Conj1.C 3)
             edg 7 8 (GLL.Conj1.C 3)
             edg 8 9 (GLL.Conj1.C 3)
             edg 9 10 (GLL.Conj1.RNGLR_EOF 0)
             ] |> ignore
        let r = GLL.Conj1.buildAbstractAst qGraph
        printfn "%A" r
        match r with
        | Error str ->
            printfn "Error"
            Assert.Fail("")
        | Success tree ->
            tree.AstToDot GLL.Conj1.numToString GLL.Conj1.tokenToNumber GLL.Conj1.tokenData (outputDir + "Conj1_Success.dot")
            Assert.Pass()

    [<Test>]
    member this._52_Conj1_Fail () =
        let qGraph = new ParserInputGraph<_>(0, 9)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.Conj1.A 1)
             edg 1 2 (GLL.Conj1.A 2)
             edg 2 3 (GLL.Conj1.A 3)
             edg 3 4 (GLL.Conj1.B 3)
             edg 4 5 (GLL.Conj1.B 3)
             edg 5 6 (GLL.Conj1.C 3)
             edg 6 7 (GLL.Conj1.C 3)
             edg 7 8 (GLL.Conj1.C 3)
             edg 8 9 (GLL.Conj1.RNGLR_EOF 0)
             ] |> ignore
        let r = GLL.Conj1.buildAbstractAst qGraph
        printfn "%A" r
        match r with
        | Error str ->
            printfn "Error"
            Assert.Pass()
        | Success tree ->
            tree.AstToDot GLL.Conj1.numToString GLL.Conj1.tokenToNumber GLL.Conj1.tokenData (outputDir + "Conj1_Fail.dot")
            Assert.Fail()

    [<Test>]
    member this._53_Conj2_Success () =
        let qGraph = new ParserInputGraph<_>(0, 10)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.Conj2.A 1)
             edg 1 2 (GLL.Conj2.A 2)
             edg 2 3 (GLL.Conj2.A 3)
             edg 3 4 (GLL.Conj2.B 3)
             edg 4 5 (GLL.Conj2.C 3)
             edg 5 6 (GLL.Conj2.A 3)
             edg 6 7 (GLL.Conj2.A 3)
             edg 7 8 (GLL.Conj2.A 3)
             edg 8 9 (GLL.Conj2.B 3)
             edg 9 10 (GLL.Conj2.RNGLR_EOF 0)
             ] |> ignore
        let r = GLL.Conj2.buildAbstractAst qGraph
        printfn "%A" r
        match r with
        | Error str ->
            printfn "Error"
            Assert.Fail("")
        | Success tree ->
            tree.AstToDot GLL.Conj2.numToString GLL.Conj2.tokenToNumber GLL.Conj2.tokenData (outputDir + "Conj2_Success.dot")
            Assert.Pass()

    [<Test>]
    member this._54_Conj2_Fail () =
        let qGraph = new ParserInputGraph<_>(0, 10)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.Conj2.A 1)
             edg 1 2 (GLL.Conj2.B 2)
             edg 2 3 (GLL.Conj2.A 3)
             edg 3 4 (GLL.Conj2.B 3)
             edg 4 5 (GLL.Conj2.C 3)
             edg 5 6 (GLL.Conj2.A 3)
             edg 6 7 (GLL.Conj2.A 3)
             edg 7 8 (GLL.Conj2.A 3)
             edg 8 9 (GLL.Conj2.B 3)
             edg 9 10 (GLL.Conj2.RNGLR_EOF 0)
             ] |> ignore
        let r = GLL.Conj2.buildAbstractAst qGraph
        printfn "%A" r
        match r with
        | Error str ->
            printfn "Error"
            Assert.Pass()
        | Success tree ->
            tree.AstToDot GLL.Conj2.numToString GLL.Conj2.tokenToNumber GLL.Conj2.tokenData (outputDir + "Conj2_Fail.dot")
            Assert.Fail("")

    [<Test>]
    member this._55_Conj3_Success () =
        let qGraph = new ParserInputGraph<_>(0, 5)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.Conj3.A 1)
             edg 1 2 (GLL.Conj3.B 2)
             edg 2 3 (GLL.Conj3.A 3)
             edg 3 4 (GLL.Conj3.B 3)
             edg 4 5 (GLL.Conj3.RNGLR_EOF 0)
             ] |> ignore
        let r = GLL.Conj3.buildAbstractAst qGraph
        printfn "%A" r
        match r with
        | Error str ->
            printfn "Error"
            Assert.Fail("")
        | Success tree ->
            tree.AstToDot GLL.Conj3.numToString GLL.Conj3.tokenToNumber GLL.Conj3.tokenData (outputDir + "Conj3_Success.dot")
            Assert.Fail("")

    [<Test>]
    member this._56_Conj3_Fail () =
        let qGraph = new ParserInputGraph<_>(0, 7)
        qGraph.AddVerticesAndEdgeRange
            [edg 0 1 (GLL.Conj3.A 1)
             edg 1 2 (GLL.Conj3.A 2)
             edg 2 3 (GLL.Conj3.A 3)
             edg 3 4 (GLL.Conj3.B 3)
             edg 4 5 (GLL.Conj3.B 3)
             edg 5 6 (GLL.Conj3.B 3)
             edg 6 7 (GLL.Conj3.RNGLR_EOF 0)
             ] |> ignore
        let r = GLL.Conj3.buildAbstractAst qGraph
        printfn "%A" r
        match r with
        | Error str ->
            printfn "Error"
            Assert.Pass()
        | Success tree ->
            tree.AstToDot GLL.Conj3.numToString GLL.Conj3.tokenToNumber GLL.Conj3.tokenData (outputDir + "Conj3_Fail.dot")
            Assert.Fail("")


[<EntryPoint>]
let fs x =
    //GLLAbstractParserTests.RunTests ()
    GLLAbstractParserTestsBio.RunTests ()
    0