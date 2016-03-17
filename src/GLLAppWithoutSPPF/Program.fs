module GLLAbstractApplication
open System.IO
open System
open Microsoft.FSharp.Text
open Microsoft.FSharp.Reflection
open System.IO
open AbstractAnalysis.Common
open Yard.Generators.GLL
open Yard.Generators.GLL.AbstractParserWithoutTree 
open Yard.Generators.GLL
open GLL.SimpleAmb
open Yard.Generators.GLL.ParserCommon



let lbl tokenId = tokenId
let edg f t l = new ParserEdge<_>(f,t,lbl l)


    
let f arr tokenToNumber = Array.map (fun e -> tokenToNumber e) arr
let len (edges : BioParserEdge<'token>[]) : int[] = edges |> Array.map (fun e -> e.Tokens.Length + 1) 
let edgB b e t = new BioParserEdge<_>(b, e, t) 

let inputGraph =
        let a1 = f [|GLL.SimpleAmb.A 1|] GLL.SimpleAmb.tokenToNumber
        let a2 = f [|GLL.SimpleAmb.B 1|] GLL.SimpleAmb.tokenToNumber
        let a3 = f [|GLL.SimpleAmb.C 1|] GLL.SimpleAmb.tokenToNumber
        let a4 = f [|GLL.SimpleAmb.D 1|] GLL.SimpleAmb.tokenToNumber
        let a5 = f [|GLL.SimpleAmb.C 1|] GLL.SimpleAmb.tokenToNumber
        let a6 = f [|GLL.SimpleAmb.D 1|] GLL.SimpleAmb.tokenToNumber
        let a7 = f [|GLL.SimpleAmb.A 1|] GLL.SimpleAmb.tokenToNumber
        let a8 = f [|GLL.SimpleAmb.RNGLR_EOF 2|] GLL.SimpleAmb.tokenToNumber
        let edges = [|
            edgB 0 1 a1;
            edgB 1 2 a2;
            edgB 2 3 a3;
            edgB 3 4 a4;
            edgB 4 5 a5;
            edgB 5 6 a6;
            edgB 6 7 a7;
            edgB 7 8 a8|] 
        let initV = [|((int 0 <<< 16) ||| int 0)|]
        let qGraph = new BioParserInputGraph<_>(initV, 8, len edges, edges, 9)
        qGraph
let parser = GLL.SimpleAmb.buildAbstract
let r = parser inputGraph 10 0 0
match r with
    | ParserCommon.ParseResult.Error _ ->
        printfn "Error"     
    | ParserCommon.ParseResult.Success1 arr ->
        for s in arr do
            printfn "%d %d" s.le s.lpos
            printfn "%d %d" s.re s.rpos
            printfn "sss"


