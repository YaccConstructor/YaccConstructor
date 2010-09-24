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

type TableInterpreter(tables) =
    class
        let goto states symbol = 
            Set.map 
                (fun state -> (dict tables.gotoSet).[hash(state,symbol)])
                states            

        let getDFA state = tables.automataDict.[fst state]

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

        let getItems smb state =
            let dfa = getDFA state
            Set.filter (fun rule -> rule.FromStateID = snd state && rule.Symbol = DSymbol smb) dfa.DRules
            |> buildItem state

        let getPrevItems smb state =
            let dfa = getDFA state
            Set.filter (fun rule -> rule.ToStateID = snd state && rule.Symbol = DSymbol smb) dfa.DRules
            |> buildItem state

        let memoize f = 
            let t = new System.Collections.Generic.Dictionary<_,_>()
            fun (parserState) ->        
                let id = hash(parserState)
                let key = parserState
                if t.ContainsKey(key)       
                then             
                    t.[key] 
                else     
                    let res = f(parserState) 
                    t.Add(key,res)
                    res                     

        let rec climb = 
            memoize
                (fun parserState ->
                    let gotoSet = goto parserState.statesSet parserState.inpSymbol
                    let parserResult = parse {parserState with statesSet = gotoSet}                    
                    let resPart1 =                        
                        let possibleStates = Set.map (getItems parserState.inpSymbol.name) parserState.statesSet  |> Set.unionMany
                        let resItems =
                            Set.filter
                                (fun state ->
                                    Set.exists
                                        (fun res -> 
                                            let prevItems = getPrevItems parserState.inpSymbol.name res.rItem
                                            Set.exists ((=)state) prevItems
                                            )
                                        parserResult)
                                possibleStates

                        Set.map (fun item -> {parserResult with rItem = item.state}) resItems
                    let resPart2 = Set.empty
                        //let climbRes = climb 
                    resPart1 + resPart2)

        
        and parse = 
            memoize
                (fun parserState ->
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
                        let climbRes = climb {parserState with inpSymbol = parserState.lexer.Next(parserState.inpStream)}
                        Set.filter (fun res -> not (isFinaleState res.rItem)) climbRes 
                    resPart1 + resPart2)

        
        let run lexer lexbuf = ()

        member self.Parse lexer lexbuf = run  lexer lexbuf
    end