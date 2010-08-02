// ASTInterpretator.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

#light

namespace Yard.Core

open Microsoft.FSharp.Compiler.CodeDom
open System.Reflection

type ASTInterpretator(tables: TablesLoader) = class

    let ruleToActionMap = tables.RuleToActionMap
    
    let rec interp tree = 
        match tree with
        | AST.Node(childs,name,_value) -> 
              let _tree = REAST.createREAST childs
              printfn "%A\n" (_tree.ToString())
              let s = List.map interp childs
              (*let getVal x = 
                   let _val = interp x
                   match (_val:Value.t<_,_>).value with 
                   | Value.NodeV(x) -> _val.seqNum,(Some((_val.seqNum),x):>obj)
                   | Value.LeafV(x) -> _val.seqNum,(Some((_val.seqNum),x.value):>obj)
              let fillPrefix lst = 
                  let localMaxSeqNum = fst  (List.head lst)
                  let prefix = List.init (localMaxSeqNum) (fun i -> i,(None:>obj))
                  prefix  
              let addNone lst = 
                  let _lst = fillPrefix lst
                  let rec fill lst1 lst2 =
                    match lst1,lst2 with 
                    | ((i1,v1)::tl1) as ls1 ,(((i2,_)as p2::tl2) as ls2)-> 
                       if i2-i1>1
                       then fill ((i1+1,(None:>obj))::ls1) ls2
                       else fill (p2::ls1) tl2
                    | [],hd::tl  -> fill [hd] tl
                    | lst,[] -> List.rev lst
                  let res = fill [] (_lst@lst) 
                  res 
                   
              let _asm = Assembly.Load(Assembly.GetExecutingAssembly().FullName)
              let _type = _asm.GetType("Actions")
              let methodName = ruleToActionMap.[_value.prodNum] 
              let _action = _type.GetMethod(methodName)              
              let values = 
                 let lst = (List.map getVal childs)
                 let newPrefix = addNone lst
                 let paramCount =(_action.GetParameters()).Length
                 (List.map snd newPrefix)
                 @(List.map (fun x -> x:>obj)(List.init (paramCount - newPrefix.Length) (fun _ -> None)))
              //let typeParam = Array.of_list (List.map (fun x -> x.GetType()) values) 
              //let x = _action.GetParameters();              
              let gen_action = 
                  if _action.ContainsGenericParameters 
                  then 
                     
                     let count = _action.GetGenericArguments().Length
                     _action.MakeGenericMethod(Array.init count (fun _ -> (new System.Object()).GetType()))
                      else _action
              let args =  Array.ofList values
              let res = gen_action.Invoke(null, args)              
              {_value with value = Value.NodeV(res)}*)
              _value
        | AST.Leaf(name,value)        -> value
                
    member self.Interp tree = interp tree
    
end