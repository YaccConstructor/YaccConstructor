module Test

open YC.Parsing.Common.GraphInput
open YC.Parsing.GLL
open YC.Parsing.GLL.AbstractParser
open YC.Frontends.GrammarCombinators
open Combinators
open YC.Parsing.Common.GLL

type EdgLbl = {i:int; s:string}

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
    
    let firstTest = sppfTest testDef testGraph "p$2" 10

open Test

[<EntryPoint>]
let main argv =
    printfn "%A" firstTest
    0
