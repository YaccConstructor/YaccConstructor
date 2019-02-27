module BioParser

open System.Collections.Generic
open YC.Frontends.YardFrontend
open YC.Parsing.Common.GraphInput
open YC.Core.Conversions
open YC.Core.IL
open YC.Core
open MatrixKernels
open GraphParsing
open YC.Core.Conversions.CNFandBNF

type ParsingResult = Dictionary<Util.NonTerminal, MySparseMatrix>

type BioParser(grammar) =
    let (conversions: Conversion list) =
        [new ExpandEbnfStrict.ExpandEbnf(); new ExpandRepet.ExpandExpand(); 
         new ExpandMeta.ExpandMeta(); new ExpandInnerAlt.ExpandInnerAlt();
         new ExpandTopLevelAlt.ExpandTopLevelAlt(); new CNF()]
    let yardFE = new YardFrontend()
    let IL = yardFE.ParseGrammar grammar
    let finalIL = 
        let converted = 
            conversions |> List.fold (fun il conv -> conv.ConvertGrammar il) IL.grammar
        in {IL.Definition.info = {fileName = ""}; head = None; foot = None; grammar = converted; 
            options = Map.empty; tokens = Map.empty}
    let startN = (finalIL.grammar.Head.rules |> List.find (fun r -> r.isStart)).name.text

    let tokenizer x =
        match x with
        | "A" -> 1<token>
        | "C" -> 2<token>
        | "G" -> 3<token>
        | "T" | "U" -> 4<token>
        | _ -> 5<token>

    let buildInputGraph (input: string) =
        let edges = 
            Array.init (input.Length + 1) id
            |> Array.pairwise
            |> Array.mapi (fun i (s, e) -> new ParserEdge<_>(s, e, tokenizer (string input.[i])))
        let graph = SimpleInputGraph<_>([|0|], [|input.Length|], id)
        graph.AddVerticesAndEdgeRange edges |> ignore
        graph
        
    let printGrammar () =
        finalIL.grammar.Head.rules
        |> List.iter
            (fun r ->
                   let l =
                        match r.body with
                        | PSeq(l,_,_) ->
                            l
                            |> List.map (fun r -> r.rule)
                            |> List.map (function PToken t | PRef (t,_) -> t.text)
                            |> String.concat " "

                        | x -> failwithf "Unexpected rule type %A" x
                         
                   printfn "%s %s" r.name.text l) 

        
    member private this.parse<'MatrixType> input =
        printGrammar()    
        graphParseGPU  (buildInputGraph input) finalIL tokenizer
    
    member val StartNonTerm = startN with get

    member this.Parse (input: string) =
        let dict, _, _, _ = 
                this.parse<MySparseMatrix>  input
        in ParsingResult dict
       