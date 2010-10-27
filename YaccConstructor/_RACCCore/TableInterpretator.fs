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
                        let gt = (dict tables.gotoSet).[(*hash*)((state.itemName,state.position),DSymbol(symbol.name))]
                        Set.add  {itemName = fst gt; position = snd gt; forest=[]} buf
                    with _ -> buf)                    
                Set.empty
                states            

        let private getDFA tables state = tables.automataDict.[state.itemName]

        let buildItem state= 
            Set.map
                (fun rule -> 
                    {
                        state = state
                        symbol = 
                            match rule.Symbol with
                            | DSymbol (s) -> s
                            | _           -> failwith "Error 01"
                    })

        let getItems tables smb state =
            let dfa = getDFA tables state
            Set.filter (fun rule -> rule.FromStateID = state.position && rule.Symbol = DSymbol smb) dfa.DRules
            |> buildItem state

        let getPrevItems tables smb state =
            let dfa = getDFA tables state
            Set.filter (fun rule -> rule.ToStateID = state.position && rule.Symbol = DSymbol smb) dfa.DRules
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

        let rec climb() = 
            memoize
                (fun tables parserState ->
#if DEBUG
                    printfn "\n Climb \n  parserState=%A\n gotoset: \n state = %A \n symbol = %A \n" parserState parserState.statesSet parserState.inpSymbol

#endif
                    let gotoSet = goto tables parserState.statesSet parserState.inpSymbol
#if DEBUG
                    printfn "\n goto result = %A \n" gotoSet

#endif
                    if gotoSet.IsEmpty
                    then
                        Set.map 
                            (fun state -> 
                                { 
                                    rItem      = state
                                    rInpStream = parserState.inpStream
                                    rLexer     = parserState.lexer
                                 })
                            parserState.statesSet
                    else
                        let parserResult = (parse()) tables {parserState with statesSet = gotoSet}                          
                        let resPart1 =                        
                            let possibleStates = 
                                Set.map (getItems tables parserState.inpSymbol.name) parserState.statesSet  
                                |> Set.unionMany
                            let resItems =
                                Set.fold
                                    (fun buf state ->
                                        let filter res =                                         
                                            let prevItems = getPrevItems tables parserState.inpSymbol.name res.rItem
                                            Set.exists ((=)state) prevItems
                                        let res = Set.filter filter parserResult
                                        if not (Set.isEmpty res)
                                        then 
                                            let node item frst = 
                                                [
                                                    Node(
                                                         item.forest @ frst
                                                        ,item.itemName
                                                        ,{
                                                        id    = item.itemName
                                                        trace = []
                                                        value = NodeV 0 
                                                        }
                                                      )
                                                ]
                                            let d = Set.maxElement res
                                            Set.add 
                                                {
                                                     d with 
                                                        rItem = {state.state with forest = node state.state d.rItem.forest}
                                                }                                                         
                                                buf
                                        else buf)
                                    Set.empty
                                    possibleStates
                            resItems

                        let resPart2 =
                                Set.map 
                                    (fun res -> 
                                        (climb())
                                             tables 
                                             { 
                                                parserState with 
                                                    inpSymbol = {name = res.rItem.itemName; value = ""}
                                                    inpStream = res.rInpStream
                                                    lexer     = res.rLexer
                                                    statesSet = 
                                                        Set.map 
                                                            (fun stt -> {stt with forest = stt.forest @ res.rItem.forest})
                                                            parserState.statesSet
                                             })
                                    parserResult
                                |> Set.unionMany                         
                        resPart1 + resPart2)

        
        and parse ()= 
            memoize
                (fun tables parserState ->
#if DEBUG
                    printfn "\n Parse \n  parserState=%A" parserState
#endif                        
                    let isFinaleState state= 
                        let dfa = tables.automataDict.[state.itemName]
                        Set.exists ((=) (state.position)) dfa.DFinaleStates                    
                    let resPart1 =                         
                        Set.map 
                            (fun item -> 
                                {
                                    rItem      = item
                                    rInpStream = parserState.inpStream
                                    rLexer     = parserState.lexer
                                })
                            (Set.filter isFinaleState parserState.statesSet)
                    let resPart2 =                         
                        let nextLexeme = parserState.lexer.Next(parserState.inpStream)                                        
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
                                let fl = Set.filter (isFinaleState >> not) parserState.statesSet
                                (climb()) 
                                    tables 
                                        {
                                            parserState with 
                                                statesSet = 
                                                    fl
                                                    |> Set.map (fun stt -> {stt with forest = leaf stt})
                                                inpSymbol = nextLexeme
                                        }
                            Set.filter (fun res -> not (isFinaleState res.rItem)) climbRes 
                    resPart1 + resPart2)

        
        let run (lexer:ILexer<_,_>) lexbuf tables= 
            let res = 
                (parse()) tables
                    {
                        statesSet = Set.singleton {itemName = "s"; position = 0; forest=[]}
                        inpSymbol = {name = "";value =""}                                    
                        inpStream = lexbuf
                        lexer     = lexer

                    }
            printfn "\n result %A" res