namespace Yard.Generators.GLL.Test

open Yard.Core.IL
open Yard.Core.IL.Production
open Yard.Generators.GLL
open NUnit.Framework
open Yard.Core.Helpers
open Yard.Core.IL.Rule

open Parser
open CalcParser

[<TestFixture>]
type DebuggedTest() =
    let filename = "testfile"

    let getGrammar def : Definition.t<Source.t, Source.t>=
        let tkn text = Source.t text
        let getCase (str:string) =
            if System.Char.IsUpper (str.[0])
                then PToken (tkn str)
                else PRef (tkn str, None)
        let getElem case = { omit = false; rule = case; binding = None; checker = None }
        let rec getSeq = function
            | []          -> PSeq ([], None, None)
            | [only]      -> getCase only
            | first::rest ->
                let firstElem = getElem <| getCase first
                match getSeq rest with
                | PSeq (restElems, _, _) -> PSeq (firstElem::restElems, None, None)
                | restCase -> PSeq ([firstElem; getElem restCase], None, None)
        let rec getProductionBody productions =
            match productions with
            | [right] -> getSeq right
            | right::rest -> PAlt(getSeq right, getProductionBody rest)
            | [] -> failwith "Empty productions are not allowed"
        List.mapi (fun i (name, prods) -> 
            { name = tkn name; isStart = (i = 0) ; args = []; body = getProductionBody prods; isPublic = false; metaArgs = [] }) def
        |> defaultGrammar
                             def

    [<Test>]
    member this.Test () =
        let definition : Definition.t<Source.t, Source.t> = {
            info = { fileName = filename }
            head = None
            foot = None
            options = Map.empty
            grammar = getGrammar ["nS", [["nA"; "nS"; "D"]; ["nB"; "nS"]; []];
                                  "nA", [["A"]; ["C"]];
                                  "nB", [["A"]; ["B"]]]
        
        GLLGenerator().Generate definition |> ignore
        
    [<Test>]
    member this.TestParser () =
        Assert.True(Parser.parse [| A; A; D; Parser.EOF |])
        Assert.True(Parser.parse [| A; B; B; Parser.EOF |])
        Assert.True(Parser.parse [| C; B; A; D; D; Parser.EOF |])
        Assert.False(Parser.parse [| D; Parser.EOF |])
        Assert.False(Parser.parse [| C; Parser.EOF; |])       // input too short
        Assert.False(Parser.parse [| C; D; D; Parser.EOF; |]) // input too long

    [<Test>]
    member this.TestCalcParser () =
        // (1+3) * (2 * 3 - 4) / 10
        Assert.True(CalcParser.parse [| LBRACE; NUMBER; PLUS; NUMBER; RBRACE;
                                        MULT; LBRACE; NUMBER; MULT; NUMBER; MINUS; NUMBER; RBRACE;
                                        DIV; NUMBER; EOF |] )
