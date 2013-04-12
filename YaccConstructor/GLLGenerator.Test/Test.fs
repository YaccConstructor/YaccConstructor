namespace Yard.Generators.GLL.Test

open Yard.Core.IL
open Yard.Core.IL.Production
open Yard.Generators.GLL
open NUnit.Framework
open Yard.Core.Helpers
open Yard.Core.IL.Rule

[<TestFixture>]
type DebuggedTest() =
    let filename = "testfile"    

    let getGrammar def : Definition.t<Source.t, Source.t>=
        let tkn text = Source.t text
        let getCase (str:string) =
            if System.Char.IsUpper (str.[0])
                then PRef (tkn str, None)
                else PToken (tkn str)
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

    [<Test>]
    member this.Test () =        
        let definition : Definition.t<Source.t, Source.t> =
            getGrammar ["S", [["A"; "S"; "d"]; ["B"; "S"]; []];
                                  "A", [["a"]; ["c"]];
                                  "B", [["a"]; ["b"]]]
        
        GLLGenerator().Generate definition |> ignore

    [<Test>]
    member this.TestParser () =
        Assert.True(Parser.parse [| 5; 5; 8; 4 |])
        Assert.True(Parser.parse [| 5; 6; 6; 4 |])
        Assert.False(Parser.parse [| 8; 4 |])
        