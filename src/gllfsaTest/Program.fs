open AbstractAnalysis.Common
open System
open System.Collections.Generic

let tokenizerWithoutMinimization token =
    match token with
    | 'A' -> WithoutMinimization.r16s.A()
    (*| 'B' -> WithoutMinimization.r16s.B() 
    | 'C' -> WithoutMinimization.r16s.C()
    | 'D' -> WithoutMinimization.r16s.D()
    | 'E' -> WithoutMinimization.r16s.E()
    | 'F' -> WithoutMinimization.r16s.F()
    | 'L' -> WithoutMinimization.r16s.L()
    | 'K' -> WithoutMinimization.r16s.K()*)
    | _ -> failwith "wtf"

let tokenizerWithMinimization token =
    match token with
    | 'A' -> WithMinimization.r16s.A()
    (*| 'B' -> WithMinimization.r16s.B() 
    | 'C' -> WithMinimization.r16s.C()
    | 'D' -> WithMinimization.r16s.D()
    | 'E' -> WithMinimization.r16s.E()
    | 'F' -> WithMinimization.r16s.F()
    | 'L' -> WithMinimization.r16s.L()
    | 'K' -> WithMinimization.r16s.K()*)
    | _ -> failwith "wtf"

let mEL = 500

let graphWithout (input:string) = 
    let edges = Array.init (input.Length / mEL + (if input.Length % mEL = 0 then 0 else 1)) (fun i -> input.Substring(i*mEL, if input.Substring(i*mEL).Length < mEL then input.Substring(i*mEL).Length else mEL))
    let edges = 
        edges
        |> Array.mapi (fun i x ->
            let tag = x.ToCharArray() |> Array.map (tokenizerWithoutMinimization >> WithoutMinimization.r16s.tokenToNumber)
            new BioParserEdge(i, i+1, tag.Length, tag, 0, 0)
            )
    new BioParserInputGraph(edges)

let graphWith (input:string) = 
    let edges = Array.init (input.Length / mEL + (if input.Length % mEL = 0 then 0 else 1)) (fun i -> input.Substring(i*mEL, if input.Substring(i*mEL).Length < mEL then input.Substring(i*mEL).Length else mEL))
    let edges = 
        edges
        |> Array.mapi (fun i x ->        
            let tag = x.ToCharArray() |> Array.map (tokenizerWithMinimization >> WithMinimization.r16s.tokenToNumber)
            new BioParserEdge(i, i+1, tag.Length, tag, 0, 0)
            )
    new BioParserInputGraph(edges)
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
    let lens = [100; 200; 300; 400; 500; 1000]
    for len in lens do
        let input = genS len
        let len = input.Length
        let gwo = graphWithout input
        let gwi = graphWith input
        GC.Collect()

        let start = System.DateTime.Now
        WithoutMinimization.r16s.buildAbstract (gwo) |> ignore
        let time1 = System.DateTime.Now - start
        
        GC.Collect()
        
        let start = System.DateTime.Now
        WithMinimization.r16s.buildAbstract (gwi) |> ignore
        let time2 = System.DateTime.Now - start
        printfn "%A %A %A" len time1 time2
    
    0
