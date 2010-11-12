// TableInterpretator.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Generators._RACCGenerator


open Yard.Generators._RACCGenerator.AST

type Item<'state, 'symbol, 'tree> =
    {
        state  : 'state
        symbol : 'symbol        
    }

module  TableInterpreter = 
    let goto tables states symbol = 
        Set.fold 
            (fun buf state -> 
                try 
                    Set.filter (fun (x,y) -> x = ((state.itemName,state.position),DSymbol(symbol.name))) tables.gotoSet
                    |> Set.map snd
                    |> Set.map (fun gt -> Set.add  {itemName = fst gt; position = snd gt; forest=state.forest} buf)
                    |> Set.unionMany
                with _ -> buf)
            Set.empty
            states

    let private getDFA tables state = tables.automataDict.[state.itemName]

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

    let getItems tables smb state =
        (getDFA tables state).DRules
        |> Set.filter (fun rule -> rule.FromStateID = state.position && rule.Symbol = DSymbol smb)
        |> buildItem state

    let getPrevItems tables smb state =
        (getDFA tables state).DRules
        |> Set.filter (fun rule -> rule.ToStateID = state.position && rule.Symbol = DSymbol smb)
        |> buildItem state

    let memoize f = 
        let t = new System.Collections.Generic.Dictionary<_,_>()
        fun tables parserState ->        
            let id = hash(parserState)
            let key = parserState
            if t.ContainsKey(key)       
            then             
                t.[key] 
            else     
                let res = f tables parserState
                t.Add(key,res)
                res                     

    let print ps =
        printfn "ParseState: \n     i = %A \n      symbol = %A \n     statesSet ="     ps.i   ps.inpSymbol 
        Set.map 
            (fun s -> List.map PrintTree s.forest)
            ps.statesSet

    let rec climb() = 
        memoize
            (fun tables parserState ->
#if DEBUG
                printfn "\n Climb \n  parserState=%A\n" (print parserState)
#endif
                let gotoSet = goto tables parserState.statesSet parserState.inpSymbol
#if DEBUG
               // printfn "\n goto result = %A \n" gotoSet
#endif          
                
                let parserResult = (parse()) tables {parserState with statesSet = gotoSet}                                        
                (*if Set.isEmpty  parserResult 
                    &&  Set.exists 
                        (fun itm -> itm.itemName = "s" && itm.position = 0 && (parserState.inpStream:Lexing.LexBuffer<_>).IsPastEndOfStream ) 
                        parserState.statesSet
                then
                    Set.map 
                        (fun itm -> 
                            {
                                rItem      = itm 
                                rInpStream = parserState.inpStream
                                rLexer     = parserState.lexer
                            })
                        parserState.statesSet
                else*)
                let resPart1 =  
                    Set.map
                        (fun res ->                                
                            getPrevItems tables parserState.inpSymbol.name res.rItem
                            |> Set.fold  
                                (fun buf itm ->
                                    let _val  = 
                                        {
                                            id    = itm.state.itemName
                                            trace = []
                                            value = NodeV 1
                                        }
                                    let node f = (f , itm.state.itemName, _val ) |> Node
                                    if  //(itm.state.itemName ="s" && itm.state.position = 0 && (parserState.inpStream:Lexing.LexBuffer<_>).IsPastEndOfStream )  
                                        //||
                                        itm.state.position > 0
                                    then                                        
                                        Set.add
                                            {
                                                rItem  = {itm.state with forest = (*[node*) (parserState.statesSet.MinimumElement.forest @ res.rItem.forest)(*]*)}
                                                rI     = parserState.i
                                                rLexer = parserState.lexer
                                            }
                                            buf
                                    else 
                                        let flg = res.rItem.itemName = "s" && res.rLexer.IsEnd()
                                        (*if flg
                                        then
                                            {
                                                rItem  = {res.rItem with forest = [node parserState.statesSet.MinimumElement.forest]}
                                                rI     = parserState.i
                                                rLexer = parserState.lexer
                                            } |> Set.singleton
                                        else*)
                                        (climb())
                                            tables 
                                            { 
                                                parserState with 
                                                    inpSymbol = {name = res.rItem.itemName; value = ""} 
                                                    i = res.rI                                           
                                                    statesSet = 
                                                        Set.map 
                                                            (fun stt -> {stt with forest = [stt.forest @ res.rItem.forest |> node ]})
                                                            parserState.statesSet
                                            }
                                        |> Set.union  buf
                                )
                                Set.empty                                                              
                        )                        
                        parserResult
                    |> Set.unionMany                                                               
                let res = resPart1
                printfn "RESULT   %A" res
                res)

        
    and parse ()= 
        memoize
            (fun tables parserState ->
#if DEBUG
                printfn "\n Parse \n  parserState=%A" (print parserState)
#endif                        
                let isFinaleState state= 
                    let dfa = tables.automataDict.[state.itemName]
                    Set.exists ((=) (state.position)) dfa.DFinaleStates                    
                let resPart1 =                    
                    let buildResult item =
                        {
                            rItem  = {item with forest = []}
                            rI     = parserState.i
                            rLexer = parserState.lexer
                        }
                    Set.filter isFinaleState parserState.statesSet
                    |> Set.map buildResult
                        
                let resPart2 =     
                    let fl = 
                        parserState.statesSet
                        //Set.filter (isFinaleState >> not) parserState.statesSet
                    if false //fl.IsEmpty
                    then Set.empty
                    else
                        let nextLexeme =                    
                            try
                                    parserState.lexer.Get(parserState.i)
                            with _ -> {name = "EOF"; value = ""}
                        printfn "\n NEXT LEXEME = %A \n" nextLexeme
                        if nextLexeme.name = "EOF" 
                        then Set.empty                              
                        else
                            let leaf item =
                                [ Leaf(
                                        nextLexeme.name
                                        ,{
                                            id    = item.itemName
                                            trace = []
                                            value = LeafV nextLexeme
                                            }
                                        )]
                            let climbRes =                                 
                                    (climb()) 
                                        tables 
                                        {
                                            parserState with 
                                                statesSet = 
                                                    fl |> Set.map (fun stt -> {stt with forest =  leaf stt})
                                                inpSymbol = nextLexeme
                                                i         = parserState.i + 1
                                        }
                            Set.filter (fun res -> not (isFinaleState res.rItem)) climbRes 
                let res = resPart1 + resPart2
                printfn "RESULT   %A" res
                res)

        
    let run (lexer:ILexer<_>) lexbuf tables= 
        let res = 
            (parse()) tables
                {
                    statesSet = Set.singleton {itemName = "s"; position = 0; forest=[]}
                    inpSymbol = {name = "";value =""}                                    
                    i         = 1
                    lexer     = lexer

                }
        Set.iter 
            (fun x -> 
                printfn "\n result %A" x.rItem
                List.iter PrintTree x.rItem.forest)
            res