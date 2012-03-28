// Learn more about F# at http://fsharp.net

module RNGLRParserSimpleTest

open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
open NUnit.Framework
open Yard.Generators
open LexCommon

let run path astBuilder =
    let tokens = LexCommon.tokens(path)
    astBuilder tokens, tokens

let dir = @"../../../../Tests/RNGLR/"

let rec printAst ind (ast : MultiAST<_>) =
    let printInd num (x : 'a) =
        printf "%s" (String.replicate (num * 4) " ")
        printfn x
    match ast with
    | Term t -> printInd ind "t: %A" t
    | NonTerm l ->
        match !l with
        | [] -> ()
        | l ->  if l.Length > 1 then printInd ind "^^^^"
                l |> List.iteri 
                    (fun i x -> if i > 0 then
                                    printInd ind "----"
                                match x with
                                | Epsilon -> printInd ind "e"
                                | Inner (num, children) ->
                                    printInd ind "prod %d" num
                                    children
                                    |> Array.iter (printAst <| ind+1))
                if l.Length > 1 then printInd ind "vvvv"

[<TestFixture>]
type ``RNGLR parser tests with simple lexer`` () =

    [<Test>]
    member test.``First grammar test``() =
        let parser = RNGLR.ParseFirst.buildAst
        let path = dir + "first/input.txt"

        match run path parser with
        | Parser.Error (num, message),_ -> printfn "Error in position %d: %s" num message
        | Parser.Success mAst,_ -> mAst |> printAst 0

    [<Test>]
    member test.``List test``() =
        let parser = RNGLR.ParseList.buildAst
        let path = dir + "list/input.txt"

        match run path parser with
        | Parser.Error (num, message), _ -> printfn "Error in position %d: %s" num message
        | Parser.Success mAst,_ -> mAst |> printAst 0

    [<Test>]
    member test.``Simple Right Null test``() =
        let parser = RNGLR.ParseSimpleRightNull.buildAst
        let path = dir + "simpleRightNull/input.txt"

        match run path parser with
        | Parser.Error (num, message),_ -> printfn "Error in position %d: %s" num message
        | Parser.Success mAst,_ -> mAst |> printAst 0

    [<Test>]
    member test.``Complex Right Null test``() =
        let parser = RNGLR.ParseComplexRightNull.buildAst
        let path = dir + "complexRightNull/input.txt"

        match run path parser with
        | Parser.Error (num, message),_ -> printfn "Error in position %d: %s" num message
        | Parser.Success mAst,_ -> mAst |> printAst 0

    [<Test>]
    member test.``Expression test``() =
        let parser = RNGLR.ParseExpr.buildAst
        let path = dir + "expr/input.txt"

        match run path parser with
        | Parser.Error (num, message),_ -> printfn "Error in position %d: %s" num message
        | Parser.Success mAst,_ -> mAst |> printAst 0

    [<Test>]
    member test.``Counter test - simple for translator``() =
        let parser = RNGLR.ParseCounter.buildAst
        let path = dir + "counter/input.txt"

        match run path parser with
        | Parser.Error (num, message),_ -> printfn "Error in position %d: %s" num message
        | Parser.Success mAst,tokens ->
            mAst |> printAst 0
            printfn "Result: %A" (RNGLR.ParseCounter.translate tokens mAst)

    [<Test>]
    member test.``Calc test - simple for translator``() =
        let parser = RNGLR.ParseCalc.buildAst
        let path = dir + "calc/input.txt"

        match run path parser with
        | Parser.Error (num, message),_ -> printfn "Error in position %d: %s" num message
        | Parser.Success mAst,tokens ->
            mAst |> printAst 0
            printfn "Result: %A" (RNGLR.ParseCalc.translate tokens mAst)
