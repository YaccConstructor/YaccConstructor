module Yard.Core.Conversions.ExpandRepet

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production
open Yard.Core.IL.Production
open Mono.Addins
open Yard.Core.Namer
open TransformAux

open System

let dummyPos s = new Source.t(s)

let private expandRepet (ruleList: Rule.t<_,_> list) = 
    let toExpand = new System.Collections.Generic.Queue<Rule.t<_,_>>(List.toArray ruleList)

    let handleRepeat rule = 
        match rule with 
        | PRepet(r, a, b) ->                         
            match (a, b) with
            | Some a, Some b -> 
                let rec bodyRule acc =
                    if (acc > b) || (acc < 1) then failwith "Incorrect parameters of range for Repeat!"
                    if acc = b 
                    then
                        let newName = Namer.newName Namer.Names.repeat                                 
                        PSeq([for i in 1..acc -> {omit=false; rule=r; binding=None; checker=None }], None, None)
                    else
                        let newName = Namer.newName Namer.Names.repeat 
                        PAlt(PSeq([for i in 1..acc -> {omit=false; rule=r; binding=None; checker=None }], None, None), bodyRule (acc + 1)) 
                bodyRule a
            | _ -> failwith "Unsupported parameters in Repetion!"                      
        | _ -> failwith "Unexpected rule for Repeat!"  
           
    let rec expandBody attrs = function
        | PSeq(elements, actionCode, l) -> 
            elements |> List.fold (fun (res, attrs) elem ->
                match elem.rule with
                | PRepet _ | PSeq _ |PAlt _ | PMany _ |PSome _ |POpt _ as x -> 
                    let newName = Namer.newName Namer.Names.repeat
                    toExpand.Enqueue({name = dummyPos newName; args=attrs; body=elem.rule;
                                        isStart=false; isPublic=false; metaArgs=[]})                    
                    { elem with rule = PRef(dummyPos newName, list2opt <| createParams attrs) }
                | _ -> elem
                |> fun newElem -> newElem::res, if elem.binding.IsSome then attrs@[elem.binding.Value] else attrs
                 ) ([], attrs)
                |> fst |> List.rev
                |> fun elems -> PSeq (elems, actionCode, l)
        | PAlt(left, right) -> PAlt(expandBody attrs left, expandBody attrs right)
        | PMany x -> PMany(expandBody attrs x)
        | PSome x -> PSome(expandBody attrs x)
        | POpt x ->  POpt(expandBody attrs x)
        | PRepet _ as x -> handleRepeat x
        | PToken _ | PLiteral _  | PRef _  as x -> x
        | PPerm _ -> failwith "Unsupported rule in Repetion!"
        | PMetaRef _ -> failwith "Unsupported rule in Repetion!" //TODO
       
                  
    let expanded = ref []
    while toExpand.Count > 0 do 
        let toExpandRule = toExpand.Dequeue()

        let expandedRule =  expandBody toExpandRule.args toExpandRule.body
        expanded:= { toExpandRule with body=expandedRule} :: !expanded                                       
    List.rev !expanded    
   

[<assembly:Addin>]
[<assembly:AddinDependency ("YaccConstructor", "1.0")>]
do()

[<Extension>]
type ExpandExpand() = 
    inherit Conversion()
        override this.Name = "ExpandRepeat"
        override this.ConvertGrammar (grammar,_) = mapGrammar expandRepet grammar
