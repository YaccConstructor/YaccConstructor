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
                   match (((interp x):AST.Value.t<_,_>).value) with 
                   | AST.Value.NodeV(x) -> x :> obj
                   | AST.Value.LeafV(x) -> x.value :> obj
              let values = List.map getVal childs
              let _asm = Assembly.Load(Assembly.GetExecutingAssembly().FullName)
              let _action = _asm.GetType("Actions").GetMethod(ruleToActionMap.[_value.prodNum])
              let typeParam = Array.of_list (List.map (fun x -> x.GetType()) values)               
              let gen_action = 
                  if _action.ContainsGenericParameters 
                  then 
                     let count = _action.GetGenericArguments().Length
                     _action.MakeGenericMethod(Array.init count (fun _ -> (new System.Object()).GetType()))
                      else _action
              let res = gen_action.Invoke(null, Array.of_list values)              
              {_value with value = AST.Value.NodeV(res)}
        | AST.Leaf(name,value)        -> value
                
    member self.Interp tree = interp tree
    
end