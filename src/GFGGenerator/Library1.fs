namespace Yard.Generators.GFGGenerator

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production
open Yard.Core.IL.Definition
open Yard.Core.Checkers
open System.Linq
open System.IO
open Mono.Addins
open System
open Constraints
open System.Collections.Generic
open QuickGraph
open QuickGraph.Data
open QuickGraph.Graphviz
open QuickGraph.Graphviz.Dot;
open System.Text;
open Yard.Core.Conversions

type Production(patt:string, res:string list) = 
    let _pattern = patt
    let _result = res
    member this.pattern = _pattern
    member this.result = _result
    member this.productionToString() = 
        this.result |>  List.fold (fun r s -> r + " " + s) (this.pattern+":")

type Grammar(pd:Production list, nt:string list, t:string list, s:string) = 
      let prods = pd
      let ntrs = nt
      let trs = t
      let st = s
      member this.production = prods
      member this.nonterminals = ntrs
      member this.terminals = trs
      member this.start = st

module gr =
    let  mutable terms:string list = []
    let  mutable nonterms:string list = []
    let  mutable productions:Production list = []
    let  mutable starts = ""
    let mutable vertex:string list = []
    let mutable edge:(string*string*string) list = []    
    let mutable outPath = ""

type FileDotEngine()  =
    interface IDotEngine with
        member this.Run(imageType, dot:string, outputFileName: string) = 
            File.WriteAllText(gr.outPath, dot);
            (gr.outPath)


[<assembly:Addin>]
[<assembly:AddinDependency ("YaccConstructor", "1.0")>]
do()

[<Extension>]
type GrammarFlowGraph() = 
    inherit Generator()     
 
        override this.Name = "GrammarFlowGraph"
        override this.Constraints = [|noEbnf; noMeta; noBrackets; |]
        override this.Generate (definition, args) =
            let args = args.Split([|' ';'\t';'\n';'\r'|]) |> Array.filter ((<>) "")
            let pairs = Array.zeroCreate <| args.Length / 2
            for i = 0 to pairs.Length-1 do
                pairs.[i] <- args.[i * 2], args.[i * 2 + 1]
            for opt, value in pairs do
                match opt with
                | "-o" -> if value.Trim() <> "" then gr.outPath <- value
                | _ -> failwithf "Unknown option %A" opt
            //Some types using in program


            //Some global helper data structures

            let expandBrackets = new Conversions.ExpandBrackets.ExpandBrackets()
            let expandMeta = new Conversions.ExpandMeta.ExpandMeta()
            let expandEbnf = new Conversions.ExpandEbnfStrict.ExpandEbnf()
            let applyConversion (conversion:Conversion) loadIL = 
                {
                    loadIL
                        with grammar = conversion.ConvertGrammar (loadIL.grammar, [||])                               
                }

            //Some string and helper functions
            let listToString strings = strings |> List.fold (+) ""
            let containsString(str:string,list:string list) = List.exists (fun elem -> elem = str) list
            let one (x,_,_)=x
            let two (_,x,_)=x
            let three (_,_,x)=x
            let rec getEdgeTag s t edges= 
                match edges with
                |(ss,tt,tag)::tl when ss=s && tt=t ->tag
                |hd::tl -> getEdgeTag s t tl
                |_->"Not Found"

            //YardFrontend initialize and File reading
            let basePath = @"C:\Users\Alex\Documents\GitHub"
            let yak = definition
                        |> applyConversion expandMeta
                        |> applyConversion expandEbnf
                        |> applyConversion expandBrackets


            //Add terminals and neterminals from production
            let rec getProd (r:Rule.t<'a,'b>) rbody (rname:string) =
                let rec getProd' (list:elem<'a,'b> list)  (acc: string list) rname =
                    match list with
                    |hd::tl->match hd.rule with
                             |PToken tok -> if not(containsString(tok.ToString(),gr.nonterms)) then gr.nonterms<-(tok.ToString())::gr.nonterms; getProd' tl  (acc@[tok.ToString()]) (rname:string) else getProd' tl  (acc@[tok.ToString()]) (rname:string)
                             |PRef (ref,a) ->if not(containsString(ref.ToString(),gr.nonterms)) then gr.nonterms<-(ref.ToString())::gr.nonterms;  getProd' tl  (acc@[ref.ToString()]) (rname:string) else getProd' tl  (acc@[ref.ToString()]) (rname:string)
                             |PLiteral lit ->if not(containsString(lit.ToString(),gr.terms)) then gr.terms<-(lit.ToString())::gr.terms; getProd' tl  (acc@[lit.ToString()]) (rname:string) else getProd' tl  (acc@[lit.ToString()]) (rname:string)
                             |_->getProd' tl  (acc@[hd.rule.ToString()]) (rname:string)
                    |_-> gr.productions<-(new Production(rname,acc))::gr.productions
                    if (r.isStart) then gr.starts<-r.name.text;
                match rbody with
                |PAlt (x1,x2)->getProd r x1  rname; getProd r x2  rname;
                |PSeq (list,_,_) ->getProd' list [] rname
                |_-> ignore()

            //All grammar from IL
            let  getAllGrammar(def:Yard.Core.IL.Definition.t<_,_>) =
                def.grammar
                |> List.forall (fun module' ->
                    module'.rules
                    |> List.forall (fun r -> (getProd r (r.body) (r.name.ToString())); true;             
                        )
                    )

            getAllGrammar yak|>ignore
            gr.nonterms<-gr.starts::gr.nonterms
            let grammar = new Grammar(pd = gr.productions, nt = gr.nonterms, t = gr.terms, s = gr.starts)




            ////////////////////////////////////Build a lists of edges and vertex
            for nt in grammar.nonterminals do
                gr.vertex <- (nt+".")::gr.vertex
                gr.vertex <- ("."+nt)::gr.vertex

            for prod in grammar.production do
                let prod = prod
                let (patt,res) = prod.pattern,prod.result

                let rec getVert'(acc:string list, list:string list) =
                        match list with
                        |hd::tl -> gr.vertex<-(patt+":"+(listToString acc)+"."+(listToString (hd::tl)))::gr.vertex; 
                                   getVert'(acc@[hd],tl)
                        |_->gr.vertex<-(patt+":"+(listToString acc)+".")::gr.vertex
                getVert'([], res)

                gr.edge<-("."+patt,patt+":"+"."+listToString(res),"entry")::gr.edge;
                gr.edge<-(patt+":"+listToString(res)+".",patt+".","exit")::gr.edge;
  
                let rec getEdges(res:string list, acc:string list,patt:string) = 
                    match res with
                    |hd::tl -> match (containsString(hd, grammar.terminals)) with
                               |true->
                                    gr.edge<-(patt+":"+listToString(acc)+"."+listToString(hd::tl),patt+":"+listToString(acc@[hd])+"."+listToString(tl),"scan")::gr.edge;
                                    getEdges(tl,acc@[hd],patt)
                               |false->
                                        gr.edge<-(patt+":"+listToString(acc)+"."+listToString(hd::tl),"."+hd,"call")::gr.edge;
                                        gr.edge<-(hd+".",patt+":"+listToString(acc@[hd])+"."+listToString(tl),"return")::gr.edge;
                                        getEdges(tl,acc@[hd],patt)
                    |_->gr.edge<-gr.edge
                getEdges(res,[],patt)
            ////////////////////////////////////////////////////



            //Make graph from vertix and edges lists
            let graph = new AdjacencyGraph<string, Edge<string>>();
            for i in gr.vertex do
                graph.AddVertex(i)|>ignore
            for i in gr.edge do
                graph.AddEdge(new TaggedEdge<string,string>(one(i), two(i),three(i)))|>ignore

    
            //Initialize Graphviz algorithm
            let graphViz = new GraphvizAlgorithm<string, Edge<string>>(graph);
            let evHandler (evArgs:FormatVertexEventArgs<string>) =
                evArgs.VertexFormatter.Comment <- evArgs.VertexFormatter.Label
            let evHandler2 (evArgs:FormatEdgeEventArgs<string,Edge<string>>) =
                 let s =  evArgs.Edge.Source.ToString()
                 let t = evArgs.Edge.Target.ToString()
                 let k = getEdgeTag s t gr.edge
                 evArgs.EdgeFormatter.Label.Value <- k

            //Draw with File Dot Engine
            graphViz.FormatVertex.Add(  fun args -> evHandler args|>ignore)
            graphViz.FormatEdge.Add(fun args->evHandler2 args); 
            let output = graphViz.Generate((new FileDotEngine()), "graph.dot");
            output |>box
            

        override this.Generate definition = this.Generate (definition, "")