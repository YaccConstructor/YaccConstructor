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
    val EndV: Option<int>
    new (startV, str, positions) = {StartV = startV; AccumulatedString = str; Positions = positions; EndV = None}
    new (startV, str, positions, endV) = {StartV = startV; AccumulatedString = str; Positions = positions; EndV = endV}

let Interpret (inputFstLexer: FST<_,_>) =
    let maxV = inputFstLexer.Vertices |> Seq.max |> ref
    let visited = ResizeArray.init (!maxV + 1) (fun _ -> false)

    let stateInfo = ResizeArray.init (!maxV + 1) (fun _ -> new ResizeArray<StateInfo<_, _>>())

    let dfs vertex =
        let stackV = new Stack<_>()
        visited.[vertex] <- true
        stackV.Push(vertex)
        
        let NotEmpty x =
            if x > 0 then true else false
    
        let ExistNotVisitedVertex topV = 
            inputFstLexer.OutEdges(topV) |> Seq.exists(fun x -> not <| visited.[x.Target])

        let NotVisitedVertex topV = 
            inputFstLexer.OutEdges(topV) |> Seq.find(fun x -> not <| visited.[x.Target]) 

        let HandlingVertex topV v =
            //stateInfo.[v] <- ResizeArray.singleton (new StateInfo<_,_>(v, ))
            ()
                       
        while NotEmpty stackV.Count do 
            let topV = stackV.Peek()
            if ExistNotVisitedVertex topV 
            then
                let notVisitedEdge = NotVisitedVertex topV
                //handling (topV; notVisitedVertex)
                visited.[notVisitedEdge.Target] <- true
                stackV.Push(notVisitedEdge.Target)
            else 
                stackV.Pop() |> ignore
                 
    for v in inputFstLexer.InitState do
        dfs v            
         
    let res = new ParserInputGraph<_>()
    res