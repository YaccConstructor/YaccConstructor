module GrammarCombinator.FeatureExamples
open GrammarCombinator.Wrapper
open Core
open Combinators
open Shortcuts

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open FSharp.Quotations.Evaluator

module TestSuite =
    open Yard.Generators.GLL
    open AbstractAnalysis.Common

    let private psgll def : ParserCommon.ParserSourceGLL =
        let gll = new GLL() in
        gll.Generate(def, false) :?> ParserCommon.ParserSourceGLL

    let private ast (strs: array<string>) (pgll: ParserCommon.ParserSourceGLL) =
        AbstractParser.buildAst pgll <| new LinearInput(Array.map pgll.StringToToken strs)

    let genTree input filename =
        GrammarGenerator.generate "unique" // TODO: unique
        >> psgll
        >> ast input
        >> fun tree -> tree.AstToDot(filename)

let PairRec() =
    <@
        let rec p() = s()
        and s() = if 1 = 0 then tok "a" else tok "b" + p() <|> Eps
        in s() // can be `p()`, not `s`
    @>

let QuotationPrimitive() =
    let list itm sep =
        <@
            let lst() = Eps <|> %itm + !* (%sep + %itm)
            lst()
        @>
    let commas = list <@ tok "a" @> <@ tok "," @>
    list commas <@ tok ";" @>

let ScopeTest() =
    <@
        let str = "sth"
        let s = tok str
        in s
    @>

[<EntryPoint>]
let main _ =

    0 // return an integer exit code
