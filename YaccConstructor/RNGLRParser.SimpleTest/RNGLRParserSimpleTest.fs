// Learn more about F# at http://fsharp.net

module RNGLRParserSimpleTest

open Yard.Generators.RNGLR
open NUnit.Framework
open Yard.Generators
open LexCommon

let run path astBuilder =
    let tokens = LexCommon.tokens(path)
    astBuilder tokens

let dir = @"../../../../Tests/RNGLR/simple/"

[<TestFixture>]
type ``RNGLR parser tests with simple lexer`` () =

    [<Test>]
    member test.``Simple grammar test``() =
        let parser = RNGLR.Parse.buildAst
        let path = dir + "first/input.txt"
        //let rightValue = [ANode [ALeaf; ANode[ALeaf; ANode[ALeaf; ANode[ALeaf]]]]]

        run path parser
        //|> (fun x -> Assert.IsTrue <| compareRes x rightValue)
