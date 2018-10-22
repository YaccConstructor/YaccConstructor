module ErrorRecovery

open Yard.Generators.GLL
open Yard.Generators.GLL.ParserCommon
open AbstractAnalysis.Common
open Yard.Generators.GLL.AbstractParser
open Yard.Generators.Common.ASTGLL
open Yard.Generators.Common.ASTGLLFSA
open Yard.Generators.GLL.ParserCommon
open YC.API
open Yard.Frontends.YardFrontend
open Yard.Generators.GLL
open Yard.Core.Conversions.ExpandMeta

let grammarsDir =  @"./GLL.AbstractParser.Simple.Tests/"

let getInput epsilonTag tokenizer file = 
    let tokens = 
        System.IO.File.ReadAllLines(file)
        |> Seq.map (fun s -> s.Split [|' '|] |> Seq.map tokenizer)
        |> Seq.concat
        |> Array.ofSeq

    new LinearIputWithErrors(tokens, epsilonTag, [||])

let getParserSource grammarFile conv = 
    let fe = new YardFrontend()
    let gen = new GLL()
    generate (grammarsDir + grammarFile)
             fe gen 
             None
             conv
             [|""|]
             [] :?> ParserSourceGLL

let run grammarFile inputFile =
    let conv = [new ExpandMeta()]
    let parser = getParserSource grammarFile conv
    let input  = getInput parser.EpsilonInputTag parser.StringToToken inputFile
    let tree = buildAst parser input
    let n, e, t, amb = tree.CountCounters
    ()

