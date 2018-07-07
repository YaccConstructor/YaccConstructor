module EpsClosure

open System.Collections.Generic
open Microsoft.FSharp.Text
open AbstractAnalysis.Common

type DfaNode<'a> = 
    { Id: int;
      Name: string;
      mutable Transitions: ('a * DfaNode<'a>) list;
      IsFinal: bool
      IsStart: bool }

type MultiMap<'a,'b> = Dictionary<'a,ResizeArray<'b>>
//let LookupMultiMap (trDict:MultiMap<_,_>) a  =
//    if trDict.ContainsKey(a) then trDict.[a] else new ResizeArray<_>(20)

let AddToMultiMap (trDict:MultiMap<_,_>) a b =
    if not <| trDict.ContainsKey(a)
    then trDict.Add(a, new ResizeArray<_>(10)) 
    trDict.[a].Add b

// TODO: consider a better representation here.
type internal NfaNodeIdSetBuilder = HashSet<int>

type internal NfaNodeIdSet(nodes: NfaNodeIdSetBuilder) = 
    // BEWARE: the next line is performance critical
    let s = nodes |> Seq.toArray |> (fun arr -> Array.sortInPlaceWith compare arr; arr) // 19

    // These are all surprisingly slower:
    //let s = nodes |> Seq.toArray |> Array.sort 
    //let s = nodes |> Seq.toArray |> Array.sortWith compare // 76
    //let s = nodes |> Seq.toArray |> (fun arr -> Array.sortInPlace arr; arr) // 76

    member x.Representation = s
    member x.Elements = s 
    member x.Fold f z = Array.fold f z s
    interface System.IComparable with 
        member x.CompareTo(y:obj) = 
            let y = (y :?> NfaNodeIdSet)
            let xr = x.Representation
            let yr = y.Representation
            let c = compare xr.Length yr.Length
            if c <> 0 then c else 
            let n = yr.Length
            let rec go i = 
                if i >= n then 0 else
                let c = compare xr.[i] yr.[i]
                if c <> 0 then c else
                go (i+1) 
            go 0

    override x.Equals(y:obj) = 
        match y with 
        | :? NfaNodeIdSet as y -> 
            let xr = x.Representation
            let yr = y.Representation
            let n = yr.Length
            xr.Length = n && 
            (let rec go i = (i < n) && xr.[i] = yr.[i] && go (i+1) 
             go 0)
        | _ -> false

    override x.GetHashCode() = hash s

    member x.IsEmpty = (s.Length = 0)
    member x.Iterate f = s |> Array.iter f

type NodeSetSet = Set<NfaNodeIdSet>

let newDfaNodeId, reset = 
    let i = ref 0 
    fun () -> let res = !i in incr i; res
    , fun () -> i := 0
   
let NfaToDfa (inGraph: SimpleInputGraph<_>) (tagToToken : _ -> int<token>)= 
    reset ()
    let numNfaNodes = inGraph.VertexCount
    let rec EClosure1 (acc:NfaNodeIdSetBuilder) n = 
        if not (acc.Contains n) then 
            acc.Add n |> ignore (*GET*)
            let epsTransitions = (inGraph.OutEdges n) |> List.ofSeq |> List.filter (fun x -> Option.isNone x.Tag) |> List.map (fun e -> e.Target)
            match epsTransitions with 
            | [] -> () // this Clause is an optimization - the list is normally empty
            | tr -> 
                //printfn "n.Id = %A, #Epsilon = %d" n.Id tr.Length
                tr |> List.iter (EClosure1 acc) 

    let EClosure (moves:ResizeArray<_>) = 
        let acc = new NfaNodeIdSetBuilder(HashIdentity.Structural)
        for i in moves do
            EClosure1 acc i
        new NfaNodeIdSet(acc)

    // Compute all the immediate one-step moves for a set of NFA states, as a dictionary
    // mapping inputs to destination lists
    let ComputeMoves (nset:NfaNodeIdSet) = 
        let moves = new MultiMap<_,_>()
        nset.Iterate(fun nodeId -> 
            for e in inGraph.OutEdges nodeId do
                if e.Tag <> None then AddToMultiMap moves e.Tag e.Target)
                    //match dests with 
                    //| [] -> ()  // this Clause is an optimization - the list is normally empty
                    //| tr -> tr |> List.iter(fun dest -> AddToMultiMap moves e.Tag dest.Id))
        moves

    let acc = new NfaNodeIdSetBuilder(HashIdentity.Structural)
    EClosure1 acc inGraph.InitStates.[0]
    let nfaSet0 = new NfaNodeIdSet(acc)

    let dfaNodes = ref (Map.empty<NfaNodeIdSet,DfaNode<_>>)

    let GetDfaNode nfaSet = 
        if (!dfaNodes).ContainsKey(nfaSet) then 
            (!dfaNodes).[nfaSet]
        else 
            let dfaNode =
                { Id= newDfaNodeId()
                  Name = nfaSet.Fold (fun s nid -> string nid + "-" + s) ""
                  Transitions=[]
                  IsFinal= nfaSet.Fold (fun s nid -> s || inGraph.FinalStates.[0] = nid) false
                  IsStart = nfaSet.Fold (fun s nid -> s || inGraph.InitStates.[0] = nid) false
                             }
            //Printf.printfn "id = %d" dfaNode.Id;

            dfaNodes := (!dfaNodes).Add(nfaSet,dfaNode)
            dfaNode
            
    let workList = ref [nfaSet0]
    let doneSet = ref Set.empty

    //let count = ref 0 
    let rec Loop () = 
        match !workList with 
        | [] -> ()
        | nfaSet ::t -> 
            workList := t
            if (!doneSet).Contains(nfaSet) then 
                Loop () 
            else
                let moves = ComputeMoves nfaSet
                for (KeyValue(inp,movesForInput)) in moves do
                    assert (inp <> None)
                    let moveSet = EClosure movesForInput
                    if not moveSet.IsEmpty then 
                        //incr count
                        let dfaNode = GetDfaNode nfaSet
                        dfaNode.Transitions <- (inp, GetDfaNode moveSet) :: dfaNode.Transitions
                        (* Printf.printf "%d (%s) : %s --> %d (%s)\n" dfaNode.Id dfaNode.Name (match inp with EncodeChar c -> String.make 1 c | LEof -> "eof") moveSetDfaNode.Id moveSetDfaNode.Name;*)
                        workList := moveSet :: !workList

                doneSet := (!doneSet).Add(nfaSet)


                Loop()
    Loop();
    //Printf.printfn "count = %d" !count;
    let ruleStartNode = GetDfaNode nfaSet0
    let ruleNodes = 
        (!dfaNodes) 
        |> Seq.map (fun kvp -> kvp.Value) 
        |> Seq.toList
        |> List.sortBy (fun s -> s.Id)

    let res = 
        let graph = new SimpleInputGraph<_>((ruleNodes |> List.find (fun x -> x.IsStart)).Id, (ruleNodes |> List.find (fun x -> x.IsFinal)).Id, tagToToken)
        ruleNodes 
        |> List.collect (fun n -> n.Transitions |> List.map (fun (l,t) -> new ParserEdge<_>(n.Id, t.Id, l.Value)))
        |> graph.AddVerticesAndEdgeRange
        |> ignore
        graph

    res