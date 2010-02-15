namespace Yard.Core

open IL.Production

type Visitor<'a>
    (processSeq,processAlt,processRef,processMany,processSome,processToken,processLiteral) = class
  let rec processTree tree :'a = 
      match tree with
      | PSeq(seq,_) as n -> 
         let result = List.map (fun l -> processTree l.rule) seq
         processSeq result n
         
      | PAlt(l,r) as n -> 
         let lResult = processTree l
         let lResult = processTree r
         processAlt lResult lResult n
               
      | PMany(expr) as n  ->
         let result = processTree expr 
         processMany result n
         
      | PSome(expr) as n ->
         let result = processTree expr 
         processSome result n
         
      | PRef(_,_)  as n -> processRef n   
      | PToken(_)   as n -> processToken n
      | PLiteral(_) as n -> processLiteral n
      | _ ->  invalidArg "processTree" "Not supported union case"
end