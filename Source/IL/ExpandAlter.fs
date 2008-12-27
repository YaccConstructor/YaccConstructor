#light "off"

module ExpandAlter

open IL
open Rule
open Production
open Source
open System

let extract_one_rule (rule:Rule.t<'a,'b>) = 
    let (name,args,body,_public,metaArgs) = (rule.name,rule.args,rule.body,rule._public,rule.metaArgs)
    in
    let rec f b =
    match b
    with
    |PAlt     (a,b) -> List.concat [f a;f b] 
    |PSeq     (a,b) -> let wrap = List.unzip(List.map (fun (x:Production.elem<'c,'d>) ->
                                                (x.rule,fun rule -> 
                                                            {omit=x.omit;
                                                             rule = rule;
                                                             binding=x.binding;
                                                             checker=x.checker}))
                                             a  )              
                       in
                       let new_body = (List.map f (fst wrap))  
                       in 
                       let rec gen lst = 
                           match lst 
                           with
                           | hd::tl -> [for x in hd -> x::(List.concat(gen tl))]
                           | []     -> []
                       in
                       List.map (fun x -> PSeq ((List.map2 (fun c f -> f c) x (snd wrap)),b))(gen new_body)   
    |PRef   (a,b) as t   -> [t]
    |PLiteral (a)
    |PToken   (a) as t   -> [t]
    | _             -> (Console.WriteLine("incorrect tree for alternative expanding!");
                       failwith "incorrect tree for alternative expanding!")
    in 
    let new_body = f body
    in
    List.map (fun x -> {name = name;
                        args =args;
                        body =x;
                        _public =_public;
                        metaArgs=metaArgs}) new_body                    