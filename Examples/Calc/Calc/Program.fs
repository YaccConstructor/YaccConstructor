// Learn more about F# at http://fsharp.net

let srcGraph = "..\..\inputGraph.dot"
let src = "..\..\inputSeq.txt"

let tokens = 
    let lexbuf = Lexing.LexBuffer<_>.FromTextReader <| new System.IO.StreamReader(src)
    seq { while not lexbuf.IsPastEndOfStream do
              yield Calc.Lexer.token lexbuf }

open Graphviz4Net.Dot.AntlrParser
open System.IO
open QuickGraph
open Graphviz4Net.Dot
open System
open Calc.Parse
//open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text
open Microsoft.FSharp.Reflection

type Collections.Generic.IDictionary<'k,'v> with
    member d.TryGetValue' k = 
        let mutable res = Unchecked.defaultof<'v> 
        let exist = d.TryGetValue(k, &res)
        if exist then Some res else None
    member d.Add'(k,v) =
        if not (d.ContainsKey k) then d.Add(k,v);true else false

exception IdentToken
let getToken = 
    let nameToUnionCtor (uci:UnionCaseInfo) = (uci.Name, FSharpValue.PreComputeUnionConstructor(uci))
    let ucis = FSharpType.GetUnionCases (typeof<Token>) |> Array.map nameToUnionCtor  |> dict 
    fun (name:string) ->
    let upperName = name.ToUpperInvariant()
    ucis.TryGetValue' upperName
    |> Option.map (fun ctor ->  ctor [| name |] :?>Token)

let graph =
    let parser = AntlrParserAdapter<string>.GetParser()
    parser.Parse(new StreamReader(File.OpenRead srcGraph))

let qGraph = new AdjacencyGraph<int, TaggedEdge<_,'TokenType>>()
graph.Edges |> Seq.iter(
            fun e -> 
                let edg = e :?> DotEdge<string>
                qGraph.AddVertex(int edg.Source.Id) |> ignore
                qGraph.AddVertex(int edg.Destination.Id) |> ignore
                let tag = getToken edg.Label
                qGraph.AddEdge(new TaggedEdge<_,_>(int edg.Source.Id,int edg.Destination.Id, tag.Value)) |> ignore)



//open Yard.Generators.RNGLR.Parser    
open Yard.Generators.RNGLR.AbstractParser
open Yard.Generators.RNGLR.AST
open Calc.Parse

match buildAst qGraph with
//match buildAst tokens with
| Error (pos, token, msg, debugFuns) ->
    printfn "Error on position %d, token %A: %s" pos token msg
| Success ast ->
    ast.PrintAst()
//    let args = {
//        tokenToRange = fun _ -> Unchecked.defaultof<_>, Unchecked.defaultof<_>
//        zeroPosition = Unchecked.defaultof<_>
//        clearAST = false
//        filterEpsilons = false
//    }
//    let result:List<double> = translate args ast
//    //defaultAstToDot ast @"..\..\astFromSeq.dot"
//    defaultAstToDot ast @"..\..\astFromDot.dot"
//    printfn "%A" result