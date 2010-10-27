// TableInterpretator.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Generators._RACCGenerator

type Item<'state, 'symbol> =
    {
        state  : 'state
        symbol : 'symbol
    }

module  TableInterpreter = //() =
    //class
        let goto tables states symbol = 
            Set.fold 
                (fun buf state -> 
                    try Set.add (dict tables.gotoSet).[(*hash*)(state,DSymbol(symbol.name))] buf
                    with _ -> buf)                    
                Set.empty
                states            

        let private getDFA tables state = tables.automataDict.[fst state]

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
            Set.filter (fun rule -> rule.FromStateID = snd state && rule.Symbol = DSymbol smb) dfa.DRules
            |> buildItem state

        let getPrevItems tables smb state =
            let dfa = getDFA tables state
            Set.filter (fun rule -> rule.ToStateID = snd state && rule.Symbol = DSymbol smb) dfa.DRules
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
                    printfn "\n Climb \n  parserState=%A" parserState
                    let gotoSet = goto tables parserState.statesSet parserState.inpSymbol
                    if gotoSet.IsEmpty
                    then
                        Set.map 
                            (fun state -> 
                                { 
                                 
                                    rItem  = state
                                    rInpStream = parserState.inpStream
                                    rLexer = parserState.lexer
                                 })
                            parserState.statesSet
                    else
                        let parserResult = (parse()) tables {parserState with statesSet = gotoSet}  
                        //if (Set.exists (fun res -> res.rInpStream.IsPastEndOfStream) parserResult)
                        //then parserResult
                       // else                                      
                        let resPart1 =                        
                            let possibleStates = Set.map (getItems tables parserState.inpSymbol.name) parserState.statesSet  |> Set.unionMany
                            let resItems =
                                Set.fold
                                    (fun buf state ->
                                        let filter res =                                         
                                            let prevItems = getPrevItems tables parserState.inpSymbol.name res.rItem
                                            Set.exists ((=)state) prevItems
                                        let res = Set.filter filter parserResult
                                        if not (Set.isEmpty res)
                                        then Set.add {Set.maxElement res with rItem = state.state} buf
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
                                                    inpSymbol = {name= fst res.rItem; value = ""};(*statesSet = Set.singleton res.rItem;*) (*inpSymbol = res.rLexer.Next(res.rInpStream);*)
                                                    inpStream = res.rInpStream; lexer = res.rLexer})
                                    parserResult
                                |> Set.unionMany                         
                        resPart1 + resPart2)

        
        and parse ()= 
            memoize
                (fun tables parserState ->
                    printfn "\n Parse \n  parserState=%A" parserState
                    let isFinaleState state= 
                        let dfa = tables.automataDict.[fst state]
                        Set.exists ((=) (snd state)) dfa.DFinaleStates                    
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
                        if parserState.inpStream.IsPastEndOfStream
                        then
                            Set.empty
                        else  
                            let nextLexeme = parserState.lexer.Next(parserState.inpStream)                                        
                            if nextLexeme.name = "EOF" 
                            then Set.empty                              
                            else
                                let climbRes = (climb()) tables {parserState with statesSet=Set.filter (isFinaleState >> not) parserState.statesSet; inpSymbol = nextLexeme}
                                Set.filter (fun res -> not (isFinaleState res.rItem)) climbRes 
                    resPart1 + resPart2)

        
        let run (lexer:ILexer<_,_>) lexbuf tables= 
            let res = 
                (parse()) tables
                    {
                        statesSet = Set.singleton ("s",0)
                        inpSymbol = {name = "";value =""}
                                    //lexer.Next(lexbuf)
                        inpStream = lexbuf
                        lexer     = lexer

                    }
            printfn "\n result %A" res

     //   member self.Parse lexer lexbuf tables = run  lexer lexbuf tables
    //end