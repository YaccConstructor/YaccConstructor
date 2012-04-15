//  TableGenerator.fs contains functions for tables generation (goto, items, LFA data, etc)
//
//  Copyright 2009, 2010, 2011, 2012 Semen Grigorev <rsdpisuy@gmail.com>
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software:you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

namespace  Yard.Generators.GNESCCGenerator

open Yard.Core.IL
open Yard.Core.IL.Definition
open Yard.Core.IL.Production
open Yard.Core.IL.Rule
open Yard.Generators.GNESCCGenerator.LALR
open Yard.Generators.GNESCCGenerator.FAL
open Yard.Generators.GNESCCGenerator.CommonTypes
open Yard.Generators.GNESCCGenerator.CommonTableFunctions

open Microsoft.FSharp.Text.StructuredFormat
open Microsoft.FSharp.Text.StructuredFormat.LayoutOps

type TableGenerator (outPath: string) = 
    class        
        let textWriter = TextWriter outPath                                
        let write str = textWriter.Write str

        let symbolsEnumerator = 
            let enum = new Enumerator()
            enum.Reset Constants.gnesccSymbolEnumeratorFrom
            enum

        let symbols = ref [("NT_"+Constants.gnesccStartRuleName,Constants.gnesccStartRuleTag)]        

        let rec getAllSymbols production = 
            match production with
                | PSeq (seq,attr)   -> 
                    List.iter (fun t -> getAllSymbols t.rule) seq                    
                | PAlt (l,r)        -> 
                    getAllSymbols l
                    getAllSymbols r                    
                | PSome expr                    
                | PMany expr
                | POpt expr      -> getAllSymbols expr

                | PRef (ch,_)
                | PToken ch as x   ->
                    let prefix =
                        x |> function | PRef(_,_) -> "NT_" | PToken _ -> "T_" | _ -> ""
                    let smbName = Source.toString ch                    
                    if List.exists (fst >> (=) (prefix + smbName)) !symbols |> not
                    then
                        let tag = symbolsEnumerator.Next()
                        symbols := (prefix + smbName , tag) :: !symbols

                | PMetaRef(_,_,_)   -> failwith ("Grammar should not contains metarules. Try to use ExpandMeta conversion.")

                | x                 -> failwith ("Element " + x.ToString() + " is not supported. Try to use corresponding conversion to transform grammar.")

        let enumerator = new Enumerator()
         
        let buildDLFA (smbIdx:System.Collections.Generic.IDictionary<_,_>) production =
            let stateEnumerator = new Enumerator()
            let builder = new AtmBuilder(stateEnumerator)
            let rec build production =
                match production with
                | PSeq (seq,attr)   -> 
                    let automataLst = List.map (fun t -> build t.rule) seq
                    let seqNum = enumerator.Next()
                    List.reduce (fun x y -> builder.Concat x y (FA.Epsilon,[[]]))  automataLst
                    |> builder.Wrap (FA.Epsilon, [[TSeqS seqNum]]) (FA.Epsilon, [[TSeqE seqNum]])

                | PAlt (l,r)        -> 
                    let lAtm = build l
                    let rAtm = build r
                    let alt1Num, alt2Num = enumerator.Next(), enumerator.Next()
                    builder.Alt lAtm rAtm 
                        (FA.Epsilon, [[TAlt1S alt1Num]]) (FA.Epsilon, [[TAlt1E alt1Num]]) 
                        (FA.Epsilon, [[TAlt2S alt2Num]]) (FA.Epsilon, [[TAlt2E alt2Num]])

                | PSome expr     ->
                    let clsNum = enumerator.Next()
                    let atm = build expr
                    builder.Cls (FA.Epsilon, [[]]) (build expr) (FA.Epsilon, [[]]) (FA.Epsilon, [[]])
                    |> fun x -> builder.Concat atm x (FA.Epsilon, [])
                    |> builder.Wrap (FA.Epsilon, [[TClsS clsNum]]) (FA.Epsilon, [[TClsE clsNum]])

                | PMany expr      ->
                    let clsNum = enumerator.Next()
                    builder.Cls (FA.Epsilon, [[]]) (build expr) (FA.Epsilon, [[TClsS clsNum]]) (FA.Epsilon, [[TClsS clsNum]])

                | POpt expr        ->
                    let clsNum = enumerator.Next()
                    builder.Opt (FA.Epsilon, [[]]) (build expr) (FA.Epsilon, [[TOptS clsNum]]) (FA.Epsilon, [[TOptE clsNum]])

                | PRef (ch,_)
                | PToken ch as x   ->
                    let prefix =
                        x |> function | PRef(_,_) -> "NT_" | PToken(_) -> "T_" | _ -> ""
                    let smbName = Source.toString ch
                    let tag = smbIdx.[prefix + smbName]                        
                    let smbNum = enumerator.Next()
                    builder.Trivial (FA.AtmSymbol tag) [[]]
                    |> builder.Wrap (FA.Epsilon, [[TSmbS smbNum]]) (FA.Epsilon, [[TSmbE smbNum]])

                | PMetaRef(_,_,_)   -> failwith ("Grammar should not contains metarules. Try to use ExpandMeta conversion.")

                | x                 -> failwith ("Element " + x.ToString() + " is not supported. Try to use corresponding conversion to transform grammar.")
            
            let dv = ref -1
            let fa = build production
            fa
            |> fun fa ->
                dv := stateEnumerator.Next()
                fa.AddVerticesAndEdge(new QuickGraph.TaggedEdge<_,_>(fa.Finale.Head,!dv,(FA.Dummy,[[]])))
                |> ignore
                fa
            |> fun fa -> FA.eCls fa FA.Epsilon
            |> fun fa ->
                fa.Start  <- Some <| Seq.find (fun v -> fa.InDegree v = 0) fa.Vertices                
                fa.Finale <- 
                    Seq.filter (fun v -> fa.OutDegree v = 0) fa.Vertices                    
                    |> List.ofSeq
                fa
            
        let generatePreheader grammarName =
            write "//this tables was generated by GNESCC"
            write ("//source grammar:" + grammarName )
            write ("//date:" + System.DateTime.Now.ToString())
            write ""            
            write "module Yard.Generators.GNESCCGenerator.Tables"
            write ""
            write "open Yard.Generators.GNESCCGenerator"
            write "open Yard.Generators.GNESCCGenerator.CommonTypes"
            write ""

        let tab = "    " 
        let genType symbols =
            write "type symbol ="
            symbols
            |> Seq.iter (fst >> sprintf "%s| %s" tab >> write)

        let genTypeToTag symbols =
            write "let getTag smb ="
            sprintf "%smatch smb with" tab
            |> write
                         
            symbols
            |> Seq.iter (fun (name,tag) -> sprintf "%s| %s -> %i" tab name tag |> write)

        let genTagToName symbols =
            write "let getName tag ="
            sprintf "%smatch tag with" tab
            |> write
                         
            symbols
            |> Seq.iter (fun (name,tag) -> sprintf "%s| %i -> %s" tab tag name |> write)
            tab + "| _ -> failwith \"getName: bad tag.\"" |> write
    
        let generate grammar =
            generatePreheader grammar.info.fileName
            let publicRule = 
                try
                    List.find (fun rule -> rule._public) grammar.grammar
                with
                | :? System.Collections.Generic.KeyNotFoundException ->
                    raise StartRuleNotFound
                    
            let startRule = 
                {
                    name    = Constants.gnesccStartRuleName
                    args    = []
                    body    = PRef((publicRule.name,(0,0)),None)
                    _public = true
                    metaArgs= []
                }

            let pmap f l =
                seq { for a in l -> async { return f a } }
                |> Async.Parallel
                |> Async.RunSynchronously

            let symbolIdx = dict !symbols |> ref
            let getTag name = 
                let flg,res = (!symbolIdx).TryGetValue name
                if flg 
                then Some res 
                else
                    printfn "Seems that nonterminal %s is described but never used." name
                    None

            let dlfaMap =                 
                startRule :: grammar.grammar                
                |> fun x -> 
                    Seq.iter (fun y -> getAllSymbols y.body) x
                    symbolIdx := dict !symbols
                    x
                //|> pmap (fun x -> x.name,buildDLFA !symbolIdx x.body)                
                |> Seq.map (fun x -> x.name, buildDLFA !symbolIdx x.body)                
                |> Seq.choose 
                    (fun (x,y) -> 
                        match "NT_" + x |> getTag with
                        | Some i -> Some (i,y)
                        | None   -> None)
                |> List.ofSeq

            let terminals,nonTerminals = 
                List.partition
                    (fun (x:string,_) -> x.StartsWith "T_")
                    !symbols
                |> fun (x,y) -> List.map snd x, List.map snd y

            let specDlfas =
                let traceEnumerator = new Enumerator()
                let traceCollection = new System.Collections.Generic.Dictionary<_,_> (1000)
                let mkNewTag (smb,trace) = 
                    let traceNum = traceEnumerator.Next()
                    traceCollection.Add(traceNum,trace)
                    match smb with 
                    | FA.AtmSymbol smb -> if List.exists ((=)smb) terminals then Terminal smb else NonTerminal smb
                    | FA.Dummy         -> Dummy
                    | FA.Epsilon       -> failwith "Incorrect dfla. Seems it contains epsilon transitions."
                    , traceNum
                dlfaMap
                |> Seq.map //pmap
                    (fun (i,dlfa) ->
                        let specFA = new FA.FA<_,_>()
                        dlfa.Edges 
                        |> Seq.iter 
                            (fun eg -> 
                                new QuickGraph.TaggedEdge<_,_>(eg.Source, eg.Target, mkNewTag eg.Tag)
                                |> specFA.AddVerticesAndEdge
                                |> ignore)
                        specFA.Start <- dlfa.Start
                        specFA.Finale <- dlfa.Finale
                        i, specFA)

                , traceCollection

            let spec = 
                {
                    Terminals = terminals
                    NonTerminals = nonTerminals
                    Productions = fst specDlfas |> Seq.map Production |> List.ofSeq
                    StartSymbols = [Constants.gnesccStartRuleTag]
                }

            genType !symbols
            genTypeToTag !symbols
            genTagToName !symbols

            let resLALR = CompilerLalrParserSpec spec
            
            let layoutTable table layoutItem tabName =
                Yard.Core.Layouts.LayoutTable table layoutItem
                |> fun l -> (wordL tabName) @@-- l
                |> Display.layout_to_string {FormatOptions.Default with PrintWidth = 500}

            let nameToIdx = 
               match resLALR with
               |(prodTab, states, startKernelIdxs, actionTable, gotoTable, etIdx, nonTerminals, symbolIdx, kernlToItemIdx, ntTab)
                ->  let isStart =
                        states
                        |> Array.mapi 
                            (fun i _ ->                                 
                                [for (pi,Production(_,_)) in prodTab.AllProds() ->
                                    let isStart =
                                        kernlToItemIdx.[i]
                                        |> Set.exists 
                                            (fun j ->
                                                let dot = dotIdx_of_item0 j
                                                let pri = prodIdx_of_item0 j                                                  
                                                pri = pi && dot = 0)
                                    pi, isStart]
                                |> List.sortBy fst
                                |> List.map snd)                        

                    let prodToNTerm = 
                        prodTab.AllProds()
                        |> List.map (fun (i,Production(nt,_)) -> i, ntTab.ToIndex nt)
                        |> List.sortBy fst
                        |> List.map snd
                            
                    Yard.Core.Layouts.LayoutArray (string >> wordL) prodToNTerm
                    |> fun l -> (wordL "let prodToNTerm = ") @@-- l
                    |> Display.layout_to_string {FormatOptions.Default with PrintWidth = 4000}
                    |> write

                    Yard.Core.Layouts.LayoutArray (string >> wordL) symbolIdx
                    |> fun l -> (wordL "let symbolIdx = ") @@-- l
                    |> Display.layout_to_string {FormatOptions.Default with PrintWidth = 4000}
                    |> write
                                                              
                    listL (string >> wordL) startKernelIdxs
                    |> fun l -> (wordL "let startKernelIdxs = ") --- l
                    |> Display.layout_to_string {FormatOptions.Default with PrintWidth = 40}
                    |> write
                     
                    layoutTable isStart (string >> fun x -> x.ToLower()) "let isStart ="
                    |> write

                    layoutTable gotoTable (function Some x -> "Some " + x.ToString() | None -> "None") "let gotoTable ="
                    |> write

                    layoutTable 
                        actionTable
                        (
                            Set.ofList
                            >> Set.map 
                                (function
                                 | _,Error    -> "Error"
                                 | _,Accept   -> "Accept" 
                                 | _,Shift i  -> "Shift " + i.ToString() 
                                 | _,Reduce i -> "Reduce " + i.ToString()
                                 )
                            >> String.concat "; "    
                            >> fun l -> "[" + l + "]")
                        "let actionTable = "
                    |> write
                    
                    ["StartIdx=startKernelIdxs"
                    ; "SymbolIdx=symbolIdx"
                    ; "GotoTable=gotoTable"
                    ; "ActionTable=actionTable"
                    ; "IsStart=isStart"
                    ; "ProdToNTerm=prodToNTerm"]
                    |> List.map wordL
                    |> aboveListL
                    |> braceL
                    |> fun l -> (wordL "let tables = ") @@-- l
                    |> Display.layout_to_string {FormatOptions.Default with PrintWidth = 40}
                    |> write
                    !symbols
                    |> Seq.map 
                        (fun (x,y) -> 
                            x, if x.StartsWith "NT_" then prodTab.NonTerminal(ntTab.ToIndex y) else y) 
                    |> dict
                                         
            textWriter.CloseOutStream ()
            nameToIdx
            ,resLALR

        member self.Generate grammar =
            generate grammar
            |> fst

        member self.DbgGenerate grammar  = 
            generate grammar
    end