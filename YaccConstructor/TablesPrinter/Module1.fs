module Yard.Development.Tools.TablesPrinter

open Yard.Generators.GNESCCGenerator.LALR
open Yard.Generators.GNESCCGenerator.CommonTypes
open Microsoft.FSharp.Text.StructuredFormat
open Microsoft.FSharp.Text.StructuredFormat.LayoutOps

let layoutTable table layoutItem tabName =
    Yard.Core.Layouts.LayoutTable table layoutItem
    |> fun l -> (wordL tabName) @@-- l
    |> Display.layout_to_string {FormatOptions.Default with PrintWidth = 500}

let tableForPrint actionTable gotoTable =
    layoutTable
        actionTable
        (
            Set.ofList
            >> Set.map
                (function
                    | _,Error   -> "Error"
                    | _,Accept   -> "Accept" 
                    | _,Shift i  -> "Shift " + i.ToString() 
                    | _,Reduce i -> "Reduce " + i.ToString()
                    )
            >> String.concat "; "    
            >> fun l -> "["+l+"]")
        "let actionTable = "
    //|> write
//    let actionPart = 
//        actionTable
//        |> Seq.mapi
//            (fun i row ->
//                i.ToString()
//                |> Seq.singleton 
//                |> Seq.append
//                <| Seq.map 
//                    (fun x -> 
//                        match snd x with
//                        | Error  -> "Error"
//                        | Accept -> "Accept"
//                        | Shift i -> "Shift " + i.ToString()
//                        | Reduce i -> "Reduce " + i.ToString()
//                        |> fun s -> s + (String.init (10 - String.length s) (fun i -> " ")))
//                    row
//            )
//    let gotoPart = 
//        gotoTable
//        |> Seq.map
//            (Seq.map 
//                (fun x -> 
//                    match x with
//                    | Some i -> "Some " + i.ToString()
//                    | None -> "None"))
//                
//    Seq.map2 (fun x y -> Seq.append x y) actionPart gotoPart
//    |> Seq.map (String.concat "|")
//    |> String.concat "\n"

let mutable res = ""

let printToCSV 
    ((prodTab:ProductionTable),states,startKernelIdxs,
     actionTable, gotoTable,
     idx, nonTerminals, symbolIdx, kernalToItem, ntTab) =
    
    res <- tableForPrint actionTable gotoTable

    prodTab.AllProds()
    |> Seq.map 
        (function
         | x,Production(nt,fa) 
            -> let populator = QuickGraph.Glee.GleeGraphExtensions.CreateGleePopulator(fa)
               populator.Compute()
               populator.GleeGraph)

let formatRaccGenresult (g:Yard.Generators.GNESCCGenerator.GNESCCGenerator) il =         
    g.DbgGenerate il
    |> snd
    |> printToCSV

