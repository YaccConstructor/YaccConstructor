module GLLAbstractParserGFGTests

open NUnit.Framework

open Yard.Frontends.YardFrontend
open Yard.Generators.Common
open Yard.Generators.Common.GrammarFlowGraph
open Yard.Generators.GLL
open Yard.Generators.GLL.AbstractParserGFG
open Yard.Generators.GLL.ParserCommon

open GLL.AbstractParse.Emb

let buildAst = Yard.Generators.GLL.AbstractParserGFG.buildAbstractAst

[<TestFixture>]
type ``Tests`` () =
    
    let yardFE = new YardFrontend()
    let path = @"..\..\..\GLL.AbstractParserGFG.Tests\"

    [<Test>]
    member this.``test``() =
        let eqGrammar = yardFE.ParseGrammar (path + "EmbEq.yrd")
        let map = function
            | "RBR" -> RBR 0
            | "LBR" -> LBR 0
            | "A" -> A 0
            | _ -> failwith "Unexpected token"

        let graph = new GrammarFlowGraph<Token> (eqGrammar.grammar.[0].rules, map, RNGLR_EOF 0)
        //graph.PrintToDot "GFG.dot"
        let ast =  buildAst parserSource graph
        
        match ast with
        | Error str -> Assert.Fail str
        | Success tree -> 
            tree.AstToDot numToString tokenToNumber tokenData @"sppf.dot"

        Assert.IsFalse false