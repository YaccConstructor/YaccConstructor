module Test

open YC.Parsing.Common.GraphInput
open YC.Parsing.GLL
open YC.Parsing.GLL.AbstractParser
open YC.Frontends.GrammarCombinators
open Combinators
open YC.Parsing.Common.GLL
open System.Diagnostics

type EdgLbl = {i:int; s:string}

module private Utils =
    let buildTestGraph triples (ps: ParserSourceGLL) tFun =
        let allVs = triples |> Array.collect (fun (f,t,l) -> [|f * 1<positionInInput>; t * 1<positionInInput>|]) |> Set.ofArray |> Array.ofSeq
        let g = new SimpleInputGraph<string>(allVs, tFun >> ps.StringToToken)

        [| for (f, t, l) in triples -> new ParserEdge<_>(f, t, l) |]
        |> g.AddVerticesAndEdgeRange
        |> ignore

        g

    let expractPath ps graph nonTermName maxLength =
        let gss, sppf, _ = parse ps graph true
        
        let nt = sppf.GetNonTermByName nonTermName ps
        let pathset = sppf.Iterate nt ps maxLength
        pathset |> Array.ofSeq

module Test =

    let private psgll def : ParserSourceGLL =
        let gll = new GLL() in
        gll.Generate(def, false) :?> ParserSourceGLL

    let private triples = [
        (0, 1, {i=0;s="In"});
        (1, 2, {i=1;s="A"});
        (2, 1, {i=2;s="B"});
        (1, 3, {i=3;s="Out"})
    ]
      
      
    let isOne (x:EdgLbl) = x.i = 1
    let isTwo (x:EdgLbl) = x.i = 2
    let isOut (x:EdgLbl) = x.s = "Out"
        
    let testCFPQ () =
      
        <@
            let rec s() = tok isOne + s() + tok isTwo <|> Eps in
            let p() = tok (fun x -> x.s = "In") + s() + tok isOut
            in p()
        @>
    
    let testGraph (ps:ParserSourceGLL) tFun =
        let allVs =
            triples
            |> List.collect (fun (x,y,_) -> [x;y])
            |> Set.ofSeq
            |> Array.ofSeq
            |> Array.map ((*)1<positionInInput>)
        
    

            
        let g = new SimpleInputGraph<_>(allVs, fun x -> tFun x |> ps.StringToToken)

        [|for (f,t,lbl) in triples -> new ParserEdge<_>(f, t, lbl)|]
        |> g.AddVerticesAndEdgeRange
        |> ignore

        g

    let sppfTest (def,tFun) graph nonTermName maxLength =
        let ps = psgll def
        let _, sppf, _ = parse ps (graph ps tFun) true
        let nt = sppf.GetNonTermByName nonTermName ps
        
        let pathset = sppf.Iterate nt ps maxLength
//        maxLength = Seq.length pathset
        pathset

    let private testDef = testCFPQ () |> GrammarGenerator.generate<EdgLbl> "unique"
    
    let firstTest () = sppfTest testDef testGraph "p$2" 10

    let testAliased =
        let triples = [|
                (0, 1, "foo");
                (2, 1, "foo_rev")
            |]
        
        let testAliased =
            <@
                let rec aliasedTo () = tok ((=)"foo_rev") + aliasedTo () + tok ((=)"foo") + aliasedTo () <|> Eps
                in aliasedTo ()
            @>
            
        let def = testAliased |> GrammarGenerator.generate<_> "aliasedTo" 
        let ps =  fst def |> psgll
        let g = Utils.buildTestGraph triples ps (snd def)
        let path = Utils.expractPath ps g "aliasedTo$3" 10
        Debug.WriteLine("Path length = {0}", Seq.length path)
        for (x, first, last) in path do
            Debug.WriteLine("{0}: {1} -> {2}", x, first, last)

open Test

[<EntryPoint>]
let main argv =
    printfn "%A" testAliased
    0
