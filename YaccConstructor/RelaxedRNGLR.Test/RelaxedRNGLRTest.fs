module RelaxedRNGLRTest

open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
open NUnit.Framework
open Yard.Generators
open LexCommon

let run path astBuilder =
    let tokens = LexCommon.tokens(path)
    astBuilder tokens


let dir = @"../../../../Tests/RNGLR/Relaxed/"
let inline printErr (num, token : 'a, msg) =
    printfn "Error in position %d on Token %A: %s" num token msg
    Assert.Fail(sprintf "Error in position %d on Token %A: %s" num token msg)

let inline translate (f : TranslateArguments<_,_> -> 'b -> 'c) (ast : 'b) =
    let args = {
        tokenToRange = fun _ -> 0,0
        zeroPosition = 0
        clearAST = false
        filterEpsilons = true
    }
    f args ast

[<TestFixture>]
type ``Relaxed RNGLR parser tests with simple lexer`` () =
    [<Test>]
    member test.``First relaxed simple grammar test``() =
        let parser = AttendedRNGLR.Simple1.buildAst
        let path = dir + "simple_1.txt"

        match run path parser with
        | Parser.Error (num, tok, err,_) -> printErr (num, tok, err)
        | Parser.Success mAst -> mAst.PrintAst()