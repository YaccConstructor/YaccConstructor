// RegExpAST.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

module Yard.Generators.RecursiveAscent.REAST

open Yard.Generators.RecursiveAscent.AST
open Yard.Generators.RecursiveAscent

type REAST = 
   | RESeq of List<REAST>
   | REClosure of List<REAST>
   | REAlt of Option<REAST>*Option<REAST>
   | RELeaf of obj
   override self.ToString() = 
    match self with
    |RESeq(lst)     ->  "<Seq>\n"
                       + String.concat "\n    " (List.map (fun x -> (x.ToString())) lst)
                       + "</Seq>\n"
    |REClosure(lst) -> "<Closure>\n"+"</Closure>\n"
    |REAlt(a,b)     -> "<Alt>\n"+(if a.IsSome then "<Fst>\n"+a.ToString()+"</Fst>\n" else "<Snd>\n"+b.ToString()+"</Snd>\n")+"</Alt>\n" 
    |RELeaf(_val)   -> "<val = " + if _val <> null then _val.ToString() else "null" + ">\n"
   
let createREAST leafs trace =    

     let isCloser tag =
         fun tag2 ->
             match (tag,tag2) with
             | TSeqS(n1),TSeqE(n2)
             | TSmbS(n1),TSmbE(n2)
             | TAlt1S(n1),TAlt1E(n2)
             | TAlt2S(n1),TAlt2E(n2)
             | TClosureS(n1),TClosureE(n2) -> n1=n2
             | _ -> false
      
     let rec delSubTrace tag trc = 
         match trc with
         | hd :: [] -> [hd]
         | hd :: tl -> if isCloser (List.head (List.rev hd)) tag 
                       then trc 
                       else delSubTrace tag tl
         | _        -> []
                
     let rec ifilter trc =
         match trc with
         | (  TSeqS(_)  | TSmbS(_) 
            | TAlt1S(_) | TAlt2S(_)
            | TClosureS(_))::tl as hd::_ -> [hd]
            
         | hd::[] -> [hd]
         
         | (hd1::_ as hd)::hd2::[] -> if isCloser (List.head (List.rev hd2)) hd1  
                                      then trc 
                                      else [hd]
                                      
         | (hd1::_ as hd)::tl      -> if List.exists (fun subLst ->  isCloser (List.head (List.rev subLst)) hd1) tl 
                                      then hd :: (ifilter (delSubTrace hd1 tl)) 
                                      else ifilter tl
         | [] -> []         
         | _  -> failwith "Trace Bug"
         
     let isEnd x = 
         match x with
         | TSeqE(_)
         | TSmbE(_)
         | TAlt1E(_)
         | TAlt2E(_)
         | TClosureE(_) -> true 
         | _ -> false        
         
     let rec filter trc =
         match trc with
         | hd1::hd2::tl -> 
             let newHd2 = List.filter (fun elt -> List.rev elt |> List.head |> isEnd |> not ) hd2
             if List.isEmpty newHd2 
             then filter ((ifilter (hd1))::tl)             
             else 
                let newHdSet = 
                  List.rev hd2
                  |> List.map (fun elt -> (ifilter (hd1 @ [elt] )))
                  |> Set.ofList
                     
                Set.map (fun elt -> filter (elt::tl)) newHdSet
                |> List.ofSeq
                |> List.concat
         | hd::[] -> [hd]
         | _ -> []
         
         
     let rec buildForest trc childs =
         let pa =1
         in
         match trc with
         | TSmbS(i)::hd::tl -> 
            match childs with
            |hd2::tl2 -> (RELeaf hd2),tl,tl2
            | _ -> failwith "Symbol read error"
         | TAlt1S(i)::tl -> 
            let tree,nTl,chlds = buildForest tl childs
            (REAlt(Some(tree),None)),(List.tail nTl),chlds
         | TAlt2S(i)::tl -> 
            let tree,nTl,chlds = buildForest tl childs
            (REAlt(None,Some(tree))),(List.tail nTl),chlds
         | TClosureS(i)::tl -> 
            let rec pF lst nTl chdls= 
                let tree,nTl,chlds = buildForest nTl chdls
                match nTl with
                | TClosureE(j)::tl -> 
                    if j = i
                    then List.rev (tree::lst) ,tl,chlds
                    else pF (tree::lst) nTl chlds 
                | hd::tl -> pF (tree::lst) nTl chlds 
                | _ -> List.rev lst,tl,chlds
            let lst,tl,chlds = pF [] tl childs
            (REClosure(lst)),tl,chlds     
         | TSeqS(i)::tl -> 
            let rec pF lst nTl chdls= 
                let tree,nTl,chlds = buildForest nTl chdls
                match nTl with
                | TSeqE(j)::tl -> 
                    if j = i
                    then List.rev (tree::lst) ,tl,chlds
                    else pF (tree::lst) nTl chlds 
                | hd::tl -> pF (tree::lst) nTl chlds 
                | _ -> List.rev lst,tl,chlds
            let lst,tl,chlds = pF [] tl childs
            (RESeq(lst)),tl,chlds  
         | _ -> failwith "BuildForest general error" 
                     
                     
     let c = List.rev trace
             |> filter   
             |> List.sortWith (fun l1 l2 -> -1 * compare l1.Length l2.Length)              
     let filteredTrace = 
         List.rev trace
         |> filter 
         |> List.sortWith (fun l1 l2 -> -1 * compare l1.Length l2.Length)
         |> List.head
         |> List.rev 
         
     let res,_,_ = buildForest (List.concat filteredTrace) leafs
     res
   