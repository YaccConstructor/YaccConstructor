// ASTInterpretator.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Generators.RecursiveAscent

open Microsoft.FSharp.Compiler.CodeDom
open System.Reflection

type ASTInterpretator(tables: TablesLoader) = class
     
    let actionCodeModuleName = "Actions"
     
    let ruleToActionMap = tables.RuleToActionMap
    
    let rec interp tree = 
        match tree with
        | AST.Node(childs,name,_value) -> 
              let _tree = REAST.createREAST (List.map interp childs) (_value.trace)
#if DEBUG              
              printfn "%A\n" (_tree.ToString())
#endif                            
              let _asm = Assembly.Load(Assembly.GetExecutingAssembly().FullName)
              let _type = _asm.GetType(actionCodeModuleName)
              let methodName = ruleToActionMap.[_value.prodNum] 
              let _action = _type.GetMethod(methodName)                             
              let genAction = 
                  if _action.ContainsGenericParameters 
                  then                      
                     let genericArgsCount = _action.GetGenericArguments().Length
                     _action.MakeGenericMethod(Array.init genericArgsCount (fun _ -> (new System.Object()).GetType()))
                  else _action
              let args =  [|_tree :> obj|]
              let res = genAction.Invoke(null, args)              
              res
              
        | AST.Leaf(name,value) -> 
            let getVal x =                 
                match (x:Value.t<_,_>).value with 
                | Value.NodeV(x) -> x :> obj
                | Value.LeafV(x) -> x.value :> obj
                
            getVal value
             
    member self.Interp tree = interp tree
    
end