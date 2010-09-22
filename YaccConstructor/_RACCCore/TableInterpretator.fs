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
                (fun state -> tables.gotoSet.[hash(state,symbol)])
                states
            |> Set.unionMany

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

        let parse = 
            memoize
                (fun parserState ->
                    1)

        let climb = ()

        let run lexer lexbuf = ()

        member self.Parse lexer lexbuf = run  lexer lexbuf
    end