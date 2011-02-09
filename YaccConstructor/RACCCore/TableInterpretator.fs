// TableInterpretator.fs contains core functions of recursive-ascent algorithm:
//    parse and climb
//
//  Copyright 2009,2010 Semen Grigorev <rsdpisuy@gmail.com>
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

type Item<'state, 'symbol, 'tree> =
    {
        state  : 'state
        symbol : 'symbol        
    }

type ParseStatus<'a,'b,'c,'d when 'a: comparison and 'c: equality> = 
    | PSuccess of Set<AST<'a,'b,'c,'d>>
    | PError   of int

module  TableInterpreter =     
    
    let maxCorrPos = ref 0

    let Lexer: Option<ILexer<_>>  ref = ref None

    let CallCount = ref 0

    let tables,setTables =
        let Tables: Option<Tables<_,_,_,_>> ref = ref None  
        let getTables () =  (!Tables).Value
        getTables , fun t -> Tables:=t

    let goto states symbol = 
        let res = 
            Set.fold 
                (fun buf state -> 
                    let local = ref Set.empty                    
                    tables().gotoSet.TryGetValue(hash ((state.itemName,state.position),DSymbol(symbol.name)),local)
                    |> fun x -> 
                        if (not x) || Set.isEmpty (!local)
                        then 
                            buf
                        else
                            !local
                            |> (fun gt -> 
                                    Set.add 
                                        {itemName = fst gt; position = snd gt; forest=state.forest; sTrace = state.sTrace}
                                        buf
                                |> Set.map)
                            |> Set.unionMany
                 )
                Set.empty
                states   
#if DEBUG
        printfn "GOTO:\n from state : %A \nby symbol : %A \n resultset : %A\n" states symbol res     
#endif
        res

    let traceCache = new System.Collections.Generic.Dictionary<Set<FATrace list>,int>()    
    
    let private getDFA itemName = tables().automataDict.[itemName]

    let buildItem state= 
        Set.map
            (fun rule -> 
                {
                    state =  {state with position = rule.FromStateID}
                    symbol = 
                        match rule.Symbol with
                        | DSymbol (s) -> s
                        | _           -> failwith "Error 01"
                })

    let getPrevItems smb state =
                (getDFA state.itemName).DRules
                |> Set.filter (fun rule -> rule.ToStateID = state.position && rule.Symbol = DSymbol smb)
                |> buildItem state

    let traceBuilderCache = new System.Collections.Generic.Dictionary<int, int>()

    let traceEnumerator = new Enumerator()

    let cache = new System.Collections.Generic.Dictionary<_,_>()

    let memoize f =         
        fun parserState ->                    
            let key = hash (parserState.i, parserState.inpSymbol, parserState.statesSet)
            if cache.ContainsKey(key)       
            then                
                cache.[key] 
            else                
                let res = f parserState
                try
                    cache.Add(key,res)
                with 
                | :? System.ArgumentException -> ()
                res

    let print ps =
        printfn "ParseState:\n     i = %A\n     symbol = %A\n     statesSet = [\n" ps.i ps.inpSymbol        
        Set.iter 
            (fun s -> 
                printfn 
                    "         State:\n             item = %A\n             position = %A\n             forest = <<\n" 
                    s.itemName s.position                
                List.iter PrintTree s.forest
                printfn "             >>\n")
            ps.statesSet
        printfn "     ]\n" 

    let rAST =  RegExpAST()

    let nodeVal trace itm =
        let id = hash trace
        let _trace =
            if traceBuilderCache.ContainsKey(id)
            then traceBuilderCache.[id]
            else                                                
                let key = traceEnumerator.Next()
                traceBuilderCache.Add(id, key)
                traceCache.Add(rAST.BuilCorrectTrace trace,key)
                key
        {
            id    = itm.state.itemName
            trace = _trace
            value = NodeV 1
        }

    let rec climb() = 
        memoize
            (fun parserState ->
#if DEBUG
                printfn "\n Climb \n" 
                print parserState
#endif                
                               
                incr CallCount 
                let buildRes s = 
                    Set.map
                        (fun  state ->                                                                        
                            {
                                rItem  = state
                                rI     = parserState.i                                
                            }                    
                        )   
                        s                
                
                let getTrace state inpSymbol fromID toID addLabelFromDummy = 
                    let dfa = getDFA state.itemName
                    dfa.DRules
                    |> Set.filter
                           (fun rule -> rule.FromStateID = fromID && rule.ToStateID = toID && rule.Symbol = DSymbol(inpSymbol))
                    |> Set.maxElement
                    |> fun rule -> 
                        if Seq.exists ((=)rule.ToStateID) dfa.DFinaleStates && addLabelFromDummy
                        then 
                            rule.Label 
                            :: [Set.filter (fun r -> r.FromStateID = rule.ToStateID && r.Symbol = Dummy) dfa.DRules
                                |> Set.maxElement 
                                |> fun x -> x.Label]
                        else 
                            [rule.Label]

                let gotoSet = goto parserState.statesSet parserState.inpSymbol
                {parserState with statesSet = gotoSet} 
                |> parse ()
                |> Set.map 
                    (fun res ->                                
                        getPrevItems parserState.inpSymbol.name res.rItem                            
                        |> Set.fold
                            (fun buf itm ->
                                let node trace forest = (forest, itm.state.itemName, nodeVal trace itm) |> Node
                                let dfa = getDFA itm.state.itemName
                                let trace state =
                                    state.forest @ res.rItem.forest
                                    |> List.length = 1
                                    |> getTrace itm.state parserState.inpSymbol.name itm.state.position res.rItem.position
                                    |> fun x -> x @ res.rItem.sTrace
                                let s = parserState.statesSet.MaximumElement
                                if itm.state.position <> dfa.DStartState
                                then                                   
                                    {
                                        rItem  = {itm.state with
                                                    forest = s.forest @ res.rItem.forest
                                                    sTrace = trace s
                                                    }
                                        rI     = res.rI
                                    } 
                                    |> Set.singleton                                   
                                else
                                    if itm.state.itemName = Constants.raccStartRuleName
                                    then
                                        if ((!Lexer).Value.Get res.rI).name = "EOF"
                                        then                                                                                         
                                            {s with forest = [s.forest @ res.rItem.forest |> node (trace s)]
                                                    sTrace = []
                                            }
                                            |> Set.singleton
                                            |> buildRes 
                                        else Set.empty
                                    else 
                                        {                                            
                                            inpSymbol = {name = res.rItem.itemName; value = ""} 
                                            i         = res.rI
                                            statesSet =                                               
                                                {s with forest = [s.forest @ res.rItem.forest |> node (trace s)]
                                                        sTrace = []
                                                }
                                                |> Set.singleton
                                                
                                        }
                                        |>climb ()
                                |> Set.union  buf
                            )
                            Set.empty)
                |> Set.unionMany
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
               
                let resPart2 =                                                                               
                    let nextLexeme = (!Lexer).Value.Get parserState.i
                    if  nextLexeme.name = "EOF"
                    then 
                        Set.empty
                    else
                        let _val item =
                            {
                                id    = ""
                                trace = -1
                                value = LeafV nextLexeme
                            }
                        let leaf item = [(nextLexeme.name, _val item) |> Leaf]
                                                        
                        {
                            parserState with 
                                statesSet = 
                                    parserState.statesSet                                    
                                    |> Set.map (fun stt -> {stt with forest = leaf stt; sTrace=[]})
                                inpSymbol = nextLexeme
                                i         = parserState.i + 1
                        }
                        |> climb()
                let res = resPart1 + resPart2
                if res.Count <> 0 && !maxCorrPos < parserState.i
                then maxCorrPos := parserState.i
#if DEBUG
                printfn "\n parser result = %A" res
#endif
                res)

        
    let run lexer tables= 
        Lexer := Some lexer
        Some tables |> setTables
        let res = 
            parse()
                {
                    statesSet =
                        {
                            itemName = Constants.raccStartRuleName
                            position = (getDFA Constants.raccStartRuleName).DStartState
                            forest   = []
                            sTrace   = []
                        }
                        |> Set.singleton
                    inpSymbol = {name = "";value =""}
                    i         = 1
                }
            |> Set.fold
                (fun buf r ->
                    let forest = r.rItem.forest
                    if List.length  forest = 1 && r.rItem.itemName = Constants.raccStartRuleName
                    then
                        let getUserTree tree =
                            match tree with
                            | Node (childs,name,value) -> Some (List.head childs)
                            | _                        -> None

                        List.head forest
                        |> fun x ->
                            let y = getUserTree x
                            if y.IsSome
                            then Set.add y.Value buf
                            else buf
                    else buf)
                Set.empty
//#if DEBUG
        Set.iter PrintTree res
//#endif
        cache.Clear()
        traceBuilderCache.Clear()
        let trC =
            seq {for s in traceCache -> s.Value,s.Key}
            |> dict
        traceCache.Clear()
        let res = 
            if res.Count > 0
            then PSuccess res
            else PError !maxCorrPos 
             ,trC,!CallCount
        maxCorrPos := 0
        res