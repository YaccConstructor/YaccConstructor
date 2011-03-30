// TableInterpretator.fs contains core functions of recursive-ascent algorithm:
//    parse and climb
//
//  Copyright 2009, 2010, 2011 Semen Grigorev <rsdpisuy@gmail.com>
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

namespace Yard.Generators.RACCGenerator


open Yard.Generators.RACCGenerator.AST

type Item<'state, 'symbol> =
    {
        state  : 'state
        symbol : 'symbol        
    }

type ParseStatus<'a,'b,'c when 'a: comparison and 'c: equality and 'c: comparison> = 
    | PSuccess of Set<AST<'a,'b,'c>>
    | PError   of int

type  TableInterpreter<'lexemeValue when 'lexemeValue: comparison and 'lexemeValue: null>() = class
    
    let forest = ref []

    let maxCorrPos = ref 0

    let Lexer: Option<ILexer<'lexemeValue>>  ref = ref None

    let CallCount = ref 0

    let tables,setTables =
        let Tables: Option<Tables<_,_,_,_>> ref = ref None  
        let getTables () =  (!Tables).Value
        getTables , fun t -> Tables:=t

    let goto states symbol=         
        Set.fold
            (fun buf state -> 
                let local = ref Set.empty                    
                tables().gotoSet.TryGetValue( (state.itemName,state.position,symbol),local)
                |> fun x -> 
                    if (not x) || Set.isEmpty (!local)
                    then 
                        buf
                    else
                        set(seq{
                        yield!
                            seq { for gt in !local do                                         
                                        yield {itemName = fst gt; position = snd gt; forest = state.forest; sTrace = state.sTrace}
                                        yield! buf
                                }})
                            
                )
            Set.empty
            states   
#if DEBUG
        |> fun res -> printfn "GOTO:\n from state : %A \nby symbol : %A \n resultset : %A\n" states symbol res; res
#endif
        |> fun res -> set res    
    
    let getDFA itemName = tables().automataDict.[itemName]

    let buildItem state = 
        fun x -> 
            seq {
              for rule in x do
               yield
                {
                    state =  {state with position = rule.FromStateID}
                    symbol = 
                        match rule.Symbol with
                        | DSymbol (s) -> s
                        | _           -> failwith "Error 01"
            }}

    let getPrevItems smb state =
        (getDFA state.itemName).DRules
        |> Set.filter (fun rule -> rule.ToStateID = state.position && rule.Symbol = DSymbol smb)
        |> buildItem state

    let traceCache = new System.Collections.Generic.Dictionary<Set<FATrace list>,int>()

    let traceBuilderCache = new System.Collections.Generic.Dictionary<_, int>()

    let traceEnumerator = new Enumerator()

    let cache = new System.Collections.Generic.Dictionary<_,_>()

    let pEpsBuf = ref None
    let pEpsilon () =
        if (!pEpsBuf).IsNone
        then  
            List.ofSeq (tables().automataDict)
            |> Seq.filter (fun x -> Set.exists ((=)x.Value.DStartState) x.Value.DFinaleStates)
            |> Seq.map (fun x -> x.Key)
            |> fun x -> pEpsBuf := Some x
        (!pEpsBuf).Value
        

    let memoize f =         
        fun parserState ->                    
            let key = parserState
            let flg,res = cache.TryGetValue key
            if flg then res
            else
                let calculated = f parserState
                let flg,stored = cache.TryGetValue key // value can be inserted in the cache by recursive call of f 
                if not flg then cache.Add(key,calculated)
                calculated

    let print ps =
        printfn "ParseState:\n     i = %A\n     symbol = %A\n     statesSet = [\n" ps.i ps.inpSymbol        
        Set.iter 
            (fun s -> 
                printfn 
                    "         State:\n             item = %A\n             position = %A\n             forest = <<\n" 
                    s.itemName s.position                
                List.iter PrintTree (List.map (fun x -> !x)(s.forest))
                printfn "             >>\n")
            ps.statesSet
        printfn "     ]\n" 

    let rAST =  RegExpAST()

    let buildCorrectTrace trace =
        let id = hash trace        
        if traceBuilderCache.ContainsKey(id)
        then traceBuilderCache.[id]
        else                                                
            let key = traceEnumerator.Next()
            traceBuilderCache.Add(id, key)
            traceCache.Add(rAST.BuilCorrectTrace trace,key)
            key                    

    let rec climb() = 
        memoize
            (fun parserState ->
#if DEBUG
                printfn "\n Climb \n" 
                print parserState
#endif 
                incr CallCount
                let buildRes =                    
                    fun  state ->
                        {
                            rItem  = state
                            rI     = parserState.i
                        }
                    |> Set.map    

                if parserState.statesSet.IsEmpty
                then buildRes Set.empty
                else                
                let getTrace state inpSymbol fromID toID addLabelFromDummy = 
                    let dfa = getDFA state.itemName
                    dfa.DRules
                    |> Set.filter
                           (fun rule -> rule.FromStateID = fromID && rule.ToStateID = toID && rule.Symbol = DSymbol(inpSymbol))
                    |> Seq.nth 0
                    |> fun rule -> 
                        if Seq.exists ((=)rule.ToStateID) dfa.DFinaleStates && addLabelFromDummy
                        then 
                            rule.Label 
                            :: [Seq.pick  (fun r -> if r.FromStateID = rule.ToStateID && r.Symbol = Dummy then Some r else None) dfa.DRules
                                |> fun x -> x.Label]
                        else 
                            [rule.Label]

                let gotoSet = goto parserState.statesSet parserState.inpSymbol
                if parserState.statesSet.IsEmpty
                then Set.empty
                else
                {parserState with statesSet = gotoSet} 
                |> parse ()
                |> fun s ->
                   seq 
                    { 
                      for res in s do                          
                        for itm in getPrevItems parserState.inpSymbol res.rItem  do
                        let node trace forest =
                            (forest, itm.state.itemName, buildCorrectTrace trace, NodeV 1) 
                            |> Node
                        let dfa = getDFA itm.state.itemName
                        let trace state =
                            state.forest @ res.rItem.forest
                            |> List.length = 1
                            |> getTrace itm.state parserState.inpSymbol itm.state.position res.rItem.position
                            |> fun x -> x @ res.rItem.sTrace
                        let s = parserState.statesSet.MaximumElement
                        if itm.state.position <> dfa.DStartState
                        then
                            yield
                                {
                                rItem  = {itm.state with
                                            forest = s.forest @ res.rItem.forest
                                            sTrace = trace s
                                         }
                                rI     = res.rI
                                } 
                        else
                            let n =
                                let node = s.forest @ res.rItem.forest |> node (trace s)
                                forest := node :: ! forest
                                [ref node]
                            yield!
                                if itm.state.itemName = (*Constants.raccStartRuleName*)0 && ((!Lexer).Value.Get res.rI).tag = -1
                                then
                                    {s with forest = n
                                            sTrace = []
                                    }
                                    |> Set.singleton
                                    |> buildRes                                         
                                else                             
                                    {                                            
                                        inpSymbol = res.rItem.itemName 
                                        i         = res.rI
                                        statesSet =                                               
                                            {s with forest = n
                                                    sTrace = []
                                            }
                                            |> Set.singleton                                                
                                    }
                                    |> climb ()                          
                        }
                        |> set
                
#if DEBUG
                |> fun res -> printfn "\n climb result = %A" res; res
#endif
                |> fun res -> res)
        
    and parse () = 
        memoize 
            (fun parserState ->
#if DEBUG
                printfn "\n Parse \n" 
                print parserState
#endif                          
                incr CallCount    
                let isFinaleState state= 
                    let dfa = getDFA state.itemName
                    Set.exists ((=) state.position) dfa.DFinaleStates
                let resPart1 =
                    let buildResult item =
                        {
                            rItem  = {item with forest = []; sTrace = []}
                            rI     = parserState.i                            
                        }
                    Set.filter isFinaleState parserState.statesSet
                    |> Set.map buildResult
               
                let resPart3 =                    
                    pEpsilon ()
                    |> Seq.map 
                        (fun nextLexeme ->
                            let trace = 
                                let dfa = getDFA nextLexeme
                                dfa.DRules
                                |> Set.filter (fun x -> x.FromStateID = dfa.DStartState && x.Symbol = Dummy)
                                |> Set.map (fun x -> x.Label)
                                |> Set.unionMany
                                |> Set.filter (function |[FATrace(TSeqS _);FATrace(TClsS _);FATrace(TClsE _);FATrace(TSeqE _)]
                                                        |[FATrace(TSeqS _);FATrace(TOptS _);FATrace(TOptE _);FATrace(TSeqE _)] -> true | _ -> false)                                

                            let emptyNode = 
                                ([],nextLexeme, buildCorrectTrace [trace], NodeV nextLexeme)
                                |> Node
                            forest := emptyNode :: ! forest
                            {
                                parserState with 
                                    statesSet = 
                                        parserState.statesSet                                    
                                        |> Set.map (fun stt -> {stt with forest = [ref emptyNode]; sTrace=[]})
                                    inpSymbol = nextLexeme
                                    i         = parserState.i
                            }
                            |> climb())
                    |> Set.unionMany

                let resPart2 =                                                                               
                    let nextLexeme = (!Lexer).Value.Get parserState.i
                    if  nextLexeme.tag = -1
                    then 
                        Set.empty
                    else                   
                        let leaf item = 
                            let l = (nextLexeme.tag, LeafV nextLexeme) |> Leaf
                            forest := l :: !forest
                            [ref l]
                                                        
                        {
                            parserState with 
                                statesSet = 
                                    parserState.statesSet                                    
                                    |> Set.map (fun stt -> {stt with forest = leaf stt; sTrace=[]})
                                inpSymbol = nextLexeme.tag
                                i         = parserState.i + 1
                        }
                        |> climb()
                let res = resPart1 + resPart2 + resPart3

                if res.Count > 0 && !maxCorrPos < parserState.i
                then maxCorrPos := parserState.i
                

#if DEBUG
                printfn "\n parser result = %A" res
#endif
                res)

        
    let run lexer tables = 
        Lexer := Some lexer
        Some tables |> setTables
        let res = 
            parse()
                {
                    statesSet =
                        {
                            itemName = 0(*Constants.raccStartRuleName*)
                            position = (getDFA 0(*Constants.raccStartRuleName*)).DStartState
                            forest   = []
                            sTrace   = []
                        }
                        |> Set.singleton
                    inpSymbol = -2
                    i         = 1
                }
            |> Set.fold
                (fun buf r ->
                    let forest = List.map (fun x -> !x) (r.rItem.forest)
                    if List.length  forest = 1 && r.rItem.itemName = 0(*Constants.raccStartRuleName*)
                    then
                        let getUserTree tree =
                            match tree with
                            | Node (childs,_,_,_) -> Some (List.head childs)
                            | _                        -> None

                        List.head forest
                        |> fun x ->
                            let y = getUserTree x
                            if y.IsSome
                            then Set.add !y.Value buf
                            else buf
                    else buf)
                Set.empty
#if DEBUG
        Set.iter PrintTree res
#endif
        cache.Clear()
        traceBuilderCache.Clear()
        let trC =
            seq {for s in traceCache -> s.Value,s.Key}
            |> dict
        traceCache.Clear()
        let res = 
            if res.Count > 0
            then PSuccess res
            else PError (!maxCorrPos + 1)
             ,trC,!CallCount
        CallCount := 0
        maxCorrPos := 0
        res
    member self.Run lexer tables = run lexer tables
end