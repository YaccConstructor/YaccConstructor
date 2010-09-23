// TableInterpretator.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Generators._RACCGenerator

type TableInterpreter(tables) =
    class
        let goto states symbol = 
            Set.map 
                (fun state -> (dict tables.gotoSet).[hash(state,symbol)])
                states            

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

        let climb = 
            memoize
                (fun parserState ->
                    let isFinaleState state= 
                        let dfa = tables.automataDict.[fst state]
                        Set.exists ((=) (snd state)) dfa.DFinaleStates
                    let resPart1 = Set.filter isFinaleState parserState.statesSet
                    let resPart2 = Set.empty
                        //let climbRes = climb 
                    resPart1 + resPart2)

        
        let parse = 
            memoize
                (fun parserState ->
                    let isFinaleState state= 
                        let dfa = tables.automataDict.[fst state]
                        Set.exists ((=) (snd state)) dfa.DFinaleStates
                    let resPart1 = {parserState Set.filter isFinaleState parserState.statesSet
                    let resPart2 = 
                        let climbRes = climb {parserState with inpSymbol = parserState.lexer.Next(parserState.inpStream)}
                        Set.filter (not isFinaleState) 
                    resPart1 + resPart2)

        
        let run lexer lexbuf = ()

        member self.Parse lexer lexbuf = run  lexer lexbuf
    end