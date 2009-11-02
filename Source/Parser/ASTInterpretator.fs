// ASTInterpretator.fs
//
// Copyright 2009 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

#light

namespace Yard.Core

open Microsoft.FSharp.Compiler.CodeDom
open System.CodeDom.Compiler
open System.Reflection

type ASTInterpretator(tables: Tables) = class

    let ruleToActionMap = tables.RuleToActionMap
    
    let rec interp tree = 
        match tree with
        | AST.Node(childs,name,_value) -> 
              let getVal x = 
                   let _val = interp x
                   match (_val:AST.Value.t<_,_>).value with 
                   | AST.Value.NodeV(x) -> [(_val.seqNum:>obj);(x:>obj)] 
                   | AST.Value.LeafV(x) -> [(_val.seqNum:>obj);(x.value:>obj)] 
              let values = List.concat (List.map getVal childs)
              let _asm = Assembly.Load(Assembly.GetExecutingAssembly().FullName)
              let _type = _asm.GetType("Actions")
              let methodName = ruleToActionMap.[_value.prodNum] 
              let _action = _type.GetMethod(methodName)
              let typeParam = Array.of_list (List.map (fun x -> x.GetType()) values)               
              let gen_action = 
                  if _action.ContainsGenericParameters 
                  then 
                     let count = _action.GetGenericArguments().Length
                     _action.MakeGenericMethod(Array.init count (fun _ -> (new System.Object()).GetType()))
                      else _action
              let args =  Array.of_list values
              let res = gen_action.Invoke(null, args)              
              {_value with value = AST.Value.NodeV(res)}
        | AST.Leaf(name,value)        -> value
                
    member self.Interp tree = interp tree
    
end