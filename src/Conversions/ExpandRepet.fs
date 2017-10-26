module Yard.Core.Conversions.ExpandRepet

open Yard.Core
open Yard.Core.IL
open Yard.Core.Namer
open TransformAux

open System

let dummyPos s = new Source.t(s)

let private expandRepet (ruleList: Rule<_,_> list) = 
    let toExpand = new System.Collections.Generic.Queue<Rule<_,_>>(List.toArray ruleList)

    let rec bodyRule acc b rule =
        if (acc > b) || (acc < 1) then failwith "Incorrect parameters of range for Repeat!"
        if acc = b 
        then PSeq([for i in 1..acc -> {omit=false; rule=rule; binding=None; checker=None }], None, None)
        else PAlt(PSeq([for i in 1..acc -> {omit=false; rule=rule; binding=None; checker=None }], None, None), bodyRule (acc + 1) b rule)               
            
    let handleRepeat rule = 
        match rule with 
        | PRepet(r, a, b) ->    
           match (a, b) with
            | Some a, Some b -> bodyRule a b r
            | None, Some b -> PAlt(PSeq([], None, None), bodyRule 1 b r)
            | Some a, None ->
                 let extendedBody = bodyRule a a r
                 let extendBodyMany = Seq.concat [[{omit=false; rule=extendedBody; binding=None; checker=None}]; [{omit=false; rule=(PMany r); binding=None; checker=None}]] |> Seq.toList
                 PSeq(extendBodyMany, None, None)
            | _ -> failwith "Unsupported parameters in Repetion!"                      
        | _ -> failwith "Unexpected rule for Repeat!"  
           
    let rec expandBody attrs = function        
        | PSeq(elements, actionCode, l) -> 
            PSeq(elements |>List.map (fun e -> {e with rule = expandBody attrs e.rule}) , actionCode, l)
//            elements |> List.fold (fun (res, attrs) elem ->
//                match elem.rule with
////                | PRepet _ | PSeq _ |PAlt _ | PMany _ |PSome _ |POpt _ | PMetaRef _ as x -> 
////                    let newName = Namer.newName Namer.Names.repeat
////                    toExpand.Enqueue({name = dummyPos newName; args=attrs; body=elem.rule;
////                                        isStart=false; isPublic=false; metaArgs=[]})                    
////                    { elem with rule = PRef(dummyPos newName, list2opt <| createParams attrs) }
//                | _ -> elem
//                |> fun newElem -> newElem::res, if elem.binding.IsSome then attrs@[elem.binding.Value] else attrs
//                 ) ([], attrs)
//                |> fst |> List.rev
//                |> fun elems -> PSeq (elems, actionCode, l)
        | PAlt(left, right) -> PAlt(expandBody attrs left, expandBody attrs right)
        | PConj(left, right) -> PConj(expandBody attrs left, expandBody attrs right)
        | PMany x -> PMany(expandBody attrs x)
        | PSome x -> PSome(expandBody attrs x)
        | POpt x ->  POpt(expandBody attrs x)
        | PRepet (r, a, b) as x -> 
            let newName = Namer.newName Namer.Names.repeat
            toExpand.Enqueue({name = dummyPos newName; args=attrs; body=r;
                                isStart=false; isPublic=false; isInline = false; metaArgs=[]})
            handleRepeat <| PRepet (PRef(dummyPos newName, list2opt <| createParams attrs), a, b)
        | PToken _ | PLiteral _  | PRef _  as x -> x
        | PPerm _ -> failwith "Unsupported rule in Repetion!"        
        | PMetaRef (src, args, metas) as x ->                         
            PMetaRef (src, args, metas |> List.map (fun prod -> expandBody attrs prod))                   
                                     
    let expanded = ref []
    while toExpand.Count > 0 do 
        let toExpandRule = toExpand.Dequeue()

        let expandedRule =  expandBody toExpandRule.args toExpandRule.body
        expanded:= { toExpandRule with body=expandedRule} :: !expanded                                       
    List.rev !expanded    

type ExpandExpand() = 
    inherit Conversion()
        override this.Name = "ExpandRepeat"
        override this.ConvertGrammar (grammar,_) = mapGrammar expandRepet grammar
