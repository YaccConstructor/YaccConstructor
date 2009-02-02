#light "off"

module PreCalculation

open FinitAutomata
open IL.Production
open IL.Rule
open IL.Source
open Set
open IL
open Log
open Grammar
open Grammar.Item
open Grammar.Symbol

open System

let lex_list = [PLiteral("a",(1,1));PLiteral("E",(1,1));PLiteral("+",(1,1))]

let production1 = PSeq([{omit=false;
                         rule= PToken("E",(1,1));
                         binding = None;
                         checker = None}],None)
let production2 = PSeq([{omit=false;
                         rule= PToken("E",(1,1));
                         binding = None;
                         checker = None};
                         {omit=false;
                         rule= PLiteral("+",(1,1));
                         binding = None;
                         checker = None};
                         {omit=false;
                         rule= PToken("E",(1,1));
                         binding = None;
                         checker = None}],None)
let production3 = PSeq([{omit=false;
                         rule= PLiteral("(",(1,1));
                         binding = None;
                         checker = None};
                         {omit=false;
                         rule= PToken("E",(1,1));
                         binding = None;
                         checker = None};
                         {omit=false;
                         rule= PLiteral(")",(1,1));
                         binding = None;
                         checker = None}],None)
let production4 = PSeq([{omit=false;
                         rule= PLiteral("a",(1,1));
                         binding = None;
                         checker = None}],None)
                         
let rules = 
    [ {name = "S";
       args = [];
       body = production1;
       _public = true; 
       metaArgs = []};
       {name = "E";
       args = [];
       body = production2;
       _public = true; 
       metaArgs = []};
       {name = "E";
       args = [];
       body = production3;
       _public = true; 
       metaArgs = []};
       {name = "E";
       args = [];
       body = production4;
       _public = true; 
       metaArgs = []}
     ] 

let items =
    let rules_map  = List.zip ([0..(List.length rules)-1])rules
    in
    union_all(List.map (fun (i,rl) -> let (itm,s,f) = (FA_rules(rl.body)) in
                                      print_any itm ;
                                      Console.WriteLine();
                                      print_any (s,f);
                                      Console.WriteLine();
                                      of_list(List.concat(Set.map (fun (a,b,c) ->( {prod_num = i;
                                                               prod_name = rl.name;
                                                               item_num = a;
                                                               symb = (print_any "it is in";
                                                                       match b 
                                                                       with 
                                                                        Some(PLiteral(s)|PToken(s)) -> Some(Terminal(s))
                                                                       | Some(PRef(s,e))             -> Some(Nonterminal(s))
                                                                       | _ -> failwith "error!!!");
                                                                           
                                                               next_num = Some c;
                                                               s =s;
                                                               f=f                                                                                          
                                                              }::
                                                              (if c = f
                                                               then [{prod_num = i;
                                                               prod_name = rl.name;
                                                               item_num = c;
                                                               symb = None;(*(print_any "it is in";
                                                                       match b 
                                                                       with 
                                                                        Some(PLiteral(s)|PToken(s)) -> Some(Terminal(s))
                                                                       | Some(PRef(s,e))             -> Some(Nonterminal(s))
                                                                       | _ -> failwith "error!!!");*)                                                                           
                                                               next_num = None;
                                                               s =s;
                                                               f=f}]  
                                                               else [] )))itm)))rules_map)

let getText a = 
    match a 
    with 
    |Some(Terminal(x))     ->  toString x  
    | _                    -> "" 

let closure q= 
    let rec cl i q = 
    if i = Set.count q 
    then q
    else
    let next_cl f = cl (i+1) (union_all [q;filter f items])
    in 
    next_cl (fun x -> 
             let el_for_cl = (List.nth (Set.to_list q) i)
             in 
             (try(x.prod_name = (getText el_for_cl.symb) && x.item_num=x.s)
              with ex -> false)                                            
                  )
    in
    cl 0 q

let nextItem item = 
    if item.next_num = None
    then failwith "error"
    else List.find (fun x -> item.next_num=Some(x.item_num)&&item.prod_num=x.prod_num) (to_list items)
    
let prevItem item = List.find (fun x -> Some(item.item_num)=x.next_num&&item.prod_num=x.prod_num)(to_list items)
        
let closure_set = 
     Console.WriteLine("Items:");
     print_any items;
     Console.WriteLine();
    let t = System.Collections.Generic.Dictionary<(Item.t<'a>),Set<(Item.t<'a>)>>()
    in
    Set.iter (fun x -> Console.WriteLine();
                       let cl = closure (Set.add  x empty) in
                       print_any x ; print_any " -> ";print_any (cl);
                       Console.WriteLine();t.Add(x,cl))items;
    t

let goto_set = 
    Console.WriteLine("In goto_set!!!");
    print_any closure_set;
    let eql a b = 
        match (a,b)
        with 
         (PToken(x)|PLiteral(x)),(Some(Terminal(y)|Nonterminal(y))) -> x=y
        | _ -> false
    in 
    let make_goto q x =  
        let cl = union_all (Set.map (fun x -> closure_set.[x]) q)
        in 
        (Set.map (nextItem) (Set.filter (fun item -> (eql x item.symb)) cl))
    in
    let t = System.Collections.Generic.Dictionary<(Item.t<'a>*string),Set<Item.t<'a>>>() 
    in
    let m_toString x = 
    match x 
    with
    | PToken(y)|PLiteral(y) -> toString y
    | PRef (y,z) -> toString y
    | _ -> ""
    in
    List.iter (fun x-> (Set.iter (fun y-> let gt = make_goto (add y empty) x in Console.WriteLine();
                                   print_any (y,m_toString x) ; print_any " -> ";print_any (gt);
                       Console.WriteLine();t.Add((y,m_toString x),gt)))items) lex_list;
    t  
     