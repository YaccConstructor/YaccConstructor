module YC.FST.AbstractLexing.Interpreter

open YC.FST.GraphBasedFst
open AbstractAnalysis.Common
open System.Collections.Generic
open Microsoft.FSharp.Collections
open QuickGraph

type Position<'br> =
    val pos_cnum: int
    val back_refs: 'br    

[<Struct>]
type StateInfo<'inSymb, 'br> =
    val StartV: int
    val AccumulatedString: ResizeArray<'inSymb> 
    val Positions: ResizeArray<Position<'br>>
    val mutable EndV: Option<int>
    new (startV, str, positions) = {StartV = startV; AccumulatedString = str; Positions = positions; EndV = None}
    new (startV, str, positions, endV) = {StartV = startV; AccumulatedString = str; Positions = positions; EndV = endV}

let Interpret (inputFstLexer: FST<_,_>) =   
    let maxV = inputFstLexer.Vertices |> Seq.max |> ref
    let visited = ResizeArray.init (!maxV + 1) (fun _ -> false)

    let bfs vertex (stt: StateInfo<_,_>) =
        let queueV = new Queue<_>()
        queueV.Enqueue(vertex,stt)
        
        let NotEmpty x =
            if x > 0 then true else false
        
                           
        while NotEmpty queueV.Count do 
            let flag = ref false
            let topV,curStt = queueV.Dequeue()            
            if not <| visited.[topV]
            then
                visited.[topV] <- true
                for v in inputFstLexer.OutEdges(topV) do                    
                    if v.Tag.OutSymb = Eps 
                    then   
                        //добавляем информацию
                        let newStt = new StateInfo<_,_>(curStt.StartV, ResizeArray.append curStt.AccumulatedString (ResizeArray.singleton v.Tag.InSymb), new ResizeArray<_>()) 
                        queueV.Enqueue(v.Target, newStt)
                    else
                         flag := true
                         //new StateInfo<_,_>(topV, ResizeArray.singleton v.Tag.InSymb, new ResizeArray<_>()) |> ignore

                if !flag 
                then
                    printfn 
                        "startV %i string %s" 
                        curStt.StartV 
                        (curStt.AccumulatedString 
                            |> ResizeArray.map (function | Smbl x -> x.ToString() | Eps -> "Eps" | Exclosure y -> y.ToString())
                            |> String.concat "")
              
                for v in inputFstLexer.OutEdges(topV) do                 
                    if not <| (v.Tag.OutSymb = Eps)
                    then 
                        let newStt = new StateInfo<_,_>(topV, ResizeArray.singleton v.Tag.InSymb, new ResizeArray<_>()) 
                        queueV.Enqueue(v.Target, newStt)


            else
                for v in inputFstLexer.OutEdges(topV) do                    
                    if not <| (v.Tag.OutSymb = Eps)  
                    then
                        printfn 
                            "startV %i string %s" 
                            curStt.StartV 
                            (curStt.AccumulatedString 
                                |> ResizeArray.map (function | Smbl x -> x.ToString() | Eps -> "Eps" | Exclosure y -> y.ToString())
                                |> String.concat "")
                 
    for v in inputFstLexer.InitState do
        let stt = new StateInfo<_,_>(v, new ResizeArray<_>(), new ResizeArray<_>())
        bfs v stt
         
    let res = new ParserInputGraph<_>()
    res