// Learn more about F# at http://fsharp.net

module GNESCCCoreSimpleTest

open Yard.Generators.GNESCCGenerator
open NUnit.Framework
open Yard.Generators
open LexCommon

let run path tables actions regexp getTag getName =
    let lexer = LexCommon.Lexer(path, getTag)

    try 
        for i in 1..100000000 do
            printf "(%d: %A) "
                    ((lexer :> ILexer).Get i).tag
                    (getName ((lexer :> ILexer).Get i).tag)
    with
        | _ -> ()
    printfn ""


    let parseRes = 
        let ti = new TableInterpreter(tables)
        ti.Run lexer
              
    parseRes

let dir = @"../../../../Tests/GNESCC/simple/"

[<TestFixture>]
type ``GNESCC core tests with simple lexer`` () =
    [<Test>]
    member test.``Simple grammar test``() =
        let path = dir + "first_grammar/input.txt"
        let tables     = GNESCCGenerator.Tables_first_grammar.tables
        let regexp     = GNESCC.Regexp_first_grammar.ruleToRegex
        let actions    = GNESCC.Actions_first_grammar.ruleToAction
        let rightValue = ["" |> box]
        run path tables actions regexp
                GNESCCGenerator.Tables_first_grammar.getTag
                GNESCCGenerator.Tables_first_grammar.getName
        |> printf "\nResult %A\n" 
