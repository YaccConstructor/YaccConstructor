module YC.FST.AbstractLexing.Interpreter

open YC.FST.GraphBasedFst
open AbstractAnalysis.Common
open System.Collections.Generic
open Microsoft.FSharp.Collections
open QuickGraph
open YC.FST.FstApproximation

type Position<'br> =
    val start_offset: int
    val end_offset: int
    val back_ref: 'br
    new (so, eo, br) = {start_offset = so; end_offset = eo; back_ref = br}

[<Struct>]
type StateInfo<'br> =
    val start_offset: int
    val cur_offset: int
    val curBr: 'br
    val StartV: int
    val AccumulatedString: ResizeArray<Smbl<char*'br>> 
    val Positions: ResizeArray<Position<'br>>
    val EndV: int
    new (startV, str, pos, endV, so, co, br) = 
        {
            StartV = startV
            AccumulatedString = str
            Positions = pos
            EndV = endV
            start_offset = so
            cur_offset = co
            curBr = br
        }
    member this.GetString () =
        this.AccumulatedString
        |> ResizeArray.map (function Smbl (ch,_) -> ch| x -> failwith "Symbol type %A unexpected in accumulated string." x)
        |> ResizeArray.toArray
        |> fun chs -> new string(chs)

    member this.GetPosition () = this.Positions |> ResizeArray.toArray
        
    //new (startV, str, endV, so, eo) = {StartV = startV; AccumulatedString = str; Positions = new ResizeArray<_>(); EndV = endV; start_offset = so; end_offset = eo}

let Interpret (inputFstLexer: FST<_,_>) (actions: array<StateInfo<_> -> _>) eofToken =   
    let maxV = inputFstLexer.Vertices |> Seq.max |> ref
    let visited = ResizeArray.init (!maxV + 1) (fun _ -> false)
    let edges = new ResizeArray<_>()
    let reduced = ref false
    let flag = ref false
    let bfs vertex (stt: StateInfo<_>) =
        let queueV = new Queue<_>()
        queueV.Enqueue(vertex,stt)
        
        while queueV.Count > 0 do
            let isNotActionPerformed = ref true
            let idF = ref 0
            let topV,curStt = queueV.Dequeue()
            
//            if curStt.curBr <> null && !reduce
//            then 
//                curStt.Positions.Add(new Position<_>(curStt.start_offset, curStt.cur_offset, curStt.curBr)) 
//                reduce := false           

            if not <| visited.[topV]
            then
                visited.[topV] <- true
                
                for v in inputFstLexer.OutEdges(topV) do
                    if v.Tag.OutSymb = Eps
                    then
                        //добавляем информацию                       
                        let so,co,br =
                            match v.Tag.InSymb with
                            | Smbl (x,br) -> 
                                if curStt.curBr = null || br = curStt.curBr
                                then curStt.start_offset, curStt.cur_offset + 1, br
                                else
                                    if !reduced = false then curStt.Positions.Add(new Position<_>(curStt.start_offset, curStt.cur_offset, br))
                                    if !flag = true then reduced := false
                                    0,1,br
                            | x -> failwith "Unexpected symbol in BR calculation:%A" x
                        let newStt = 
                            new StateInfo<_>(
                                curStt.StartV
                                , ResizeArray.append curStt.AccumulatedString (ResizeArray.singleton v.Tag.InSymb)
                                , curStt.Positions
                                , v.Target
                                , so
                                , co
                                , br)
                        queueV.Enqueue(v.Target, newStt)
                    else  
                        reduced := true
                        flag := true                    
                        curStt.Positions.Add(new Position<_>(curStt.start_offset, curStt.cur_offset, curStt.curBr))                         
                        if !isNotActionPerformed //один раз выполняем action код, потому что входящие дуги в одну вершину возвращают одинаковый action код
                        then                            
                            isNotActionPerformed := false
                            idF := match v.Tag.OutSymb with
                                    | Smbl x -> x
                                    | x -> failwith "Unexpected symbol in function calculation:%A" x 
                            let tok = actions.[!idF] curStt
                            edges.Add(new ParserEdge<_>(curStt.StartV, curStt.EndV, tok)) 
                            printfn 
                                "startV %i string %s" 
                                curStt.StartV 
                                (curStt.AccumulatedString 
                                    |> ResizeArray.map (function | Smbl x -> x.ToString() | Eps -> "Eps" | Exclosure y -> y.ToString())
                                    |> String.concat "") 
                                    
                     
                        let so,co,br =
                            match v.Tag.InSymb with
                                | Smbl (x,br) -> 
                                    if br = curStt.curBr
                                    then curStt.cur_offset, curStt.cur_offset + 1, curStt.curBr
                                    else 0,1,br
                                | x -> failwith "Unexpected symbol in BR calculation:%A" x
                        let newStt =
                            new StateInfo<_>(
                                topV
                                , ResizeArray.singleton v.Tag.InSymb
                                , new ResizeArray<_>()
                                , v.Target
                                , so
                                , co
                                , br) 
                        queueV.Enqueue(v.Target, newStt)    

            else
                for v in inputFstLexer.OutEdges(topV) do                    
                    if not <| (v.Tag.OutSymb = Eps)  
                    then
                    //еще раз пройтись, чтобы найти невыполненные action                        
                        idF := match v.Tag.OutSymb with
                                | Smbl x -> x
                                | x -> failwith "Unexpected symbol in function calculation:%A" x 
                        let tok = actions.[!idF] curStt
                        edges.Add(new ParserEdge<_>(curStt.StartV, curStt.EndV, tok))
                        printfn 
                            "startV %i string %s" 
                            curStt.StartV 
                            (curStt.AccumulatedString 
                                |> ResizeArray.map (function | Smbl x -> x.ToString() | Eps -> "Eps" | Exclosure y -> y.ToString())
                                |> String.concat "")
                 
    for v in inputFstLexer.InitState do
        let stt = new StateInfo<_>(v, new ResizeArray<_>(), new ResizeArray<_>(), v, 0, 0, Unchecked.defaultof<'br>)
        bfs v stt

//    let actions = [|(fun lb -> printfn lb.dsfs ; Some NUM(1) ); (fun lb -> None); (fun x -> x*2)|]
//        
//    let lb = 2
//    let tok = actions.[1] lb
//    let edg = new Edg(lb._form,tok,lb._to)
//    lb.Reset
//    lb.Add ch
    
    let final = new ResizeArray<_>()
    for edge in inputFstLexer.Edges do
        if  edge.Target = inputFstLexer.FinalState.[0]
        then final.Add edge.Source

    for v in final do 
        edges.Add(new ParserEdge<_>(v, inputFstLexer.FinalState.[0], Some eofToken))
             
    let res = new ParserInputGraph<_>(inputFstLexer.InitState.[0], inputFstLexer.FinalState.[0])
    res.AddVerticesAndEdgeRange edges |> ignore  
    res


let Tokenize (fstLexer : FST<_,_>) (actions : array<StateInfo<_> -> _>) eofToken (inputGraph : Appr<_>) =    
    let inputFst = inputGraph.ToFST()
    let inputFstLexer = FST<_,_>.Compos(inputFst, fstLexer) 
    let parserInputGraph = Interpret inputFstLexer actions eofToken 
    let epsRes = EpsClosure.NfaToDfa parserInputGraph
    epsRes 

let ToDot (parserInputGraph : ParserInputGraph<_>) filePrintPath toStr =
    let rank s l =
        "{ rank=" + s + "; " + (l |> string) + " }\n"
    let s = 
        "digraph G {\n" 
        + "rankdir = LR\n"
        + "node [shape = circle]\n"
        + sprintf "%i[style=filled, fillcolor=green]\n" parserInputGraph.InitState 
        + sprintf "%i[shape = doublecircle, style=filled, fillcolor=red]\n" parserInputGraph.FinalState
        + rank "same" parserInputGraph.InitState
        + rank "min" parserInputGraph.InitState  
        + rank "same" parserInputGraph.FinalState 
        + rank "max" parserInputGraph.FinalState
    
    let strs =
            parserInputGraph.Edges
            |> Seq.map (fun edge ->
                sprintf "%i -> %i [label=\"%s\"]; \n" edge.Source edge.Target  (toStr edge.Tag)) 
                                      
    System.IO.File.WriteAllText(filePrintPath, s + (String.concat "" strs) + "\n}")
    ()
    