open AbstractAnalysis.Common
open System
open System.Collections.Generic
open GLLFSA.test
open Yard.Generators.GLL.ParserCommon

let tokenizer token =
    match token with
    | 'A' -> GLLFSA.test.A()
    (*| 'B' -> GLLFSA.test.B() 
    | 'C' -> GLLFSA.test.C()
    | 'D' -> GLLFSA.test.D()
    | 'E' -> GLLFSA.test.E()
    | 'F' -> GLLFSA.test.F()
    | 'L' -> GLLFSA.test.L()
    | 'K' -> GLLFSA.test.K()*)
    | _ -> failwith "wtf"

let mEL = 500
(*
let graph (input:string) = 
    let edges = Array.init (input.Length / mEL + (if input.Length % mEL = 0 then 0 else 1)) (fun i -> input.Substring(i*mEL, if input.Substring(i*mEL).Length < mEL then input.Substring(i*mEL).Length else mEL))
    let edges = 
        edges
        |> Array.mapi (fun i x ->
            let tag = x.ToCharArray() |> Array.map (tokenizer >> GLLFSA.test.tokenToNumber)
            new BioParserEdge(i, i+1, tag.Length, tag, 0, 0)
            )
    new BioParserInputGraph(edges, Set[])

let graphTT = 
    let edges = [|[|5|]; [|5|]; [|5|]; [|5|]|]
    let edges = 
        edges
        |> Array.mapi (fun i tag ->        
            new BioParserEdge(i, i+1, 1, tag, 0, 0)
            )
    new BioParserInputGraph(edges, Set[0])
    *)
(*
let rec genS n = 
    let pref =  new System.Text.StringBuilder()
    let postf = new List<_>()
    for i in n..(-1)..0 do
        if i = 0 then
            pref.Append "TLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLK" |> ignore
        elif i % 2 = 0 then
            pref.Append "C" |> ignore
            postf.Add "BLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLK"
        else
            pref.Append "M" |> ignore
            postf.Add "NLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLK"
    let res = pref.ToString() + (String.Join("",postf |> Seq.rev))
    res// + res
*)
(*
let printBinary (i: int) =
    Convert.ToString(i,2)

let rec intToBinary i =
    if i = -1
    then
        ""
    else
        printfn "%s" (printBinary (i >>> 1))
        intToBinary (i >>> 1) + string (i &&& 1)

let inline packEdgePos edge position : int  = ((int position <<< 16) ||| int edge)
let inline getEdge (packedValue : int)      = int (int packedValue &&& 0xffff)
let inline getPosOnEdge (packedValue : int) = int (uint32 packedValue >>> 16)
*)

let rec genS n = 
    let s = new System.Text.StringBuilder()
    for _ in 1..n do
        s.Append "A" |> ignore
        //s.Append "ALKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLK" |> ignore
        //s.Append "BLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLK" |> ignore
        //s.Append "CLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLK" |> ignore
        //s.Append "DLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLK" |> ignore
        //s.Append "ELKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLK" |> ignore
        //s.Append "FLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLKLK" |> ignore
    s.ToString()

[<EntryPoint>]
let main argv = 
    (*let lens = [100; 200; 300; 400; 500; 1000]
    for len in lens do
        let input = genS len
        let len = input.Length
        let gwo = graphWithout input
        let gwi = graphWith input
        GC.Collect()

        let start = System.DateTime.Now
        GLLFSA.test.buildAbstract (gwo) |> ignore
        let time1 = System.DateTime.Now - start
        
        GC.Collect()
        
        let start = System.DateTime.Now
        WithMinimization.r16s.buildAbstract (gwi) |> ignore
        let time2 = System.DateTime.Now - start
        printfn "%A %A %A" len time1 time2
    *)

    let res = 
        Yard.Generators.GLL.AbstractParserWithoutTreeFSAInput.isParsed GLLFSA.test.parserSource (new LinearInput([|6<token>;6<token>;6<token>;6<token>;6<token>|]))
        

    printfn "R=%A" res
//
//    | Success1 result -> 
//        printfn "SearchWithoutSPPF succeed"
//        for res in result do
//            ()
//            //printfn "%i %i %i %i %i" res.le res.lpos res.re res.rpos res.length
//
//    | _ -> printfn "Input parsing failed."

    0
