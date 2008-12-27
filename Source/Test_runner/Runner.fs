#light "off"

open IL
open Definition
open Grammar
open Rule
open Production
open Source
open ExpandEBNF
open System
open Dump
open ExpandMeta
open ExpandAlter

let r =PAlt(PLiteral("a",(1,1)),PLiteral("A",(1,1)))
let production1 = PSeq([{omit=false;
                         rule=PAlt(PLiteral("a",(1,1)),PLiteral("A",(1,1)));
                         binding = Some (("x",(1,1)));
                         checker = None}],Some("x",(1,1)))
let production2 = PSeq([{omit=false;
                         rule=PMany(PAlt(PLiteral("d",(1,1)),PLiteral("D",(1,1))));
                         binding = Some (("y",(2,2)));
                         checker = None}],Some("y",(2,2)))
let production3 = PSeq([{omit=false;
                         rule=PMany(PAlt(production2,PLiteral("D",(1,1))));
                         binding = Some (("y",(2,2)));
                         checker = None};
                         {omit=false;
                         rule=PMany(PAlt(production2,production2));
                         binding = Some (("y",(2,2)));
                         checker = None}],Some("y",(2,2)))
let rules = 
    [ {name = "S";
       args = [];
       body = production3;
       _public = true; 
       metaArgs = []};
       {name = "S1";
       args = [];
       body = production2;
       _public = true; 
       metaArgs = []}
     ] 
let test_grammar =  {head=Some("test");
                     grammar=[];
                     foot=Some("test")}  

let t = Console.WriteLine("!!!First!!!");List.map (dumpRule 0) rules                     
let tree = expandMetaRules (convertEBNFtoMeta rules)  
let p = Console.WriteLine("!!!Second!!!");List.map (dumpRule 0) tree
let z = List.concat(List.map extract_one_rule tree)
let h = Console.WriteLine("!!!Third!!!");List.map (dumpRule 0) z
do (Console.ReadLine();())                  