// TableInterpretator.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Generators._RACCGenerator

open Yard.Generators._RACCGenerator.AST


module ASTInterpretator = 
    let rec interp (ruleToActon:System.Collections.Generic.IDictionary<_,_>) tree = 
        let reast = RegExpAST()
        match tree with        
        | Node (childs,name,value) ->
            List.map (interp ruleToActon) childs
            |> (Set.minElement value.trace.Head |> reast.BuildREAST)
            |> fun x -> 
                   match x with 
                   | (t,_,_) -> ruleToActon.[name] t                  
        | Leaf (name,value)        -> 
            match value.value with
            | LeafV(v) -> box v
            | _        -> failwith "AST is incorrect. Leaf contains NodeV value."

        
