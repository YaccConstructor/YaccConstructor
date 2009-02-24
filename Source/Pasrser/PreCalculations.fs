#light

module PreCalculation

open IL.Production
open IL.Rule
open IL
open Grammar.Item
open Grammar.Symbol
open System

let singleton x = Set.add x Set.empty

let lex_list = [PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral("c",(1,1));PLiteral("E",(1,1));PLiteral("+",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral(")",(1,1))]

let production1 = PSeq([{omit=false;
                         rule= PToken("E",(1,1));
                         binding = None;
                         checker = None}],None)
let production2 = PSeq([{omit=false;
                         rule= PToken("E",(1,1));
                         binding = None;
                         checker = None};
                         {omit=false;
                         rule= PAlt(PLiteral("+",(1,1)),PLiteral("*",(1,1)));
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
                         rule= PMany(PAlt(PLiteral("a",(1,1)),PLiteral("b",(1,1))));
                         binding = None;
                         checker = None}],None)
let production5 = PSeq([{omit=false;
                         rule= PAlt(PAlt(production2,production3),production4);
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
       body = production5;
       _public = true; 
       metaArgs = []}
     ] 

let items =
    let rules_map  = List.zip ([0..(List.length rules)-1])rules
    List.map (fun (i,rl) -> let (itm,s,f) = (FinitAutomata.FA_rules(rl.body)) in                                      
#if DEBUG
                                      Set.iter print_any itm ;
                                      Console.WriteLine();
                                      print_any (s,f);
                                      Console.WriteLine();
#endif
                                      Set.of_list(List.concat(Set.map (fun (a,b,c) ->
                                                             ( {prod_num = i;
                                                               prod_name = rl.name;
                                                               item_num = a;
                                                               symb = (match b with 
                                                                        Some(PLiteral(s)|PToken(s)|PRef(s,_)) -> Some(Terminal(s))                                                                                  
                                                                       | _ -> failwith "error!!!");                                                                           
                                                               next_num = Some c;
                                                               s =s;
                                                               f=f                                                                                          
                                                              }::
                                                              (if (Set.exists ((=)c) f)
                                                               then [{prod_num = i;
                                                               prod_name = rl.name;
                                                               item_num = c;
                                                               symb = None;                                                                    
                                                               next_num = None;
                                                               s =s;
                                                               f=f}]  
                                                               else [] )))itm)))rules_map
    |> Set.union_all

let getText = function
    |Some( Terminal x )     ->  Source.toString x  
    | _                     -> "" 

let closure q = 
    let rec cl i q = 
      if i = Set.count q 
      then q
      else
        let next_cl f = cl (i+1) (Set.union_all [q; Set.filter f items])
        next_cl (fun x -> 
                 let el_for_cl = List.nth (Set.to_list q) i 
                 x.prod_name = getText el_for_cl.symb && x.item_num = x.s)                                                                                 
    in
    cl 0 q

let nextItem item = 
    let isNext x = item.next_num = Some x.item_num && item.prod_num=x.prod_num
    Set.filter isNext items
    
let prevItem item = 
    let isPrev x = Some item.item_num = x.next_num && item.prod_num = x.prod_num
    Set.filter isPrev items
        
let closure_set = 
#if DEBUG
  Console.WriteLine("Items:");
  Set.iter print_any items;
  Console.WriteLine();
#endif
  dict <| Set.map (fun x -> x, closure (singleton x) ) items
  
let goto_set =     
    let eql = function 
        | (PToken x |PLiteral x), Some(Terminal y | Nonterminal y ) -> x=y
        | _ -> false
    in 
    let make_goto q x =  
        let cl = Set.union_all (Set.map (fun x -> closure_set.[x]) q)         
        Set.union_all(Set.map (nextItem) (Set.filter (fun item -> (eql (x ,item.symb))) cl))    
    let t = new System.Collections.Generic.Dictionary<(Grammar.Item.t<Source.t>*string),Set<Grammar.Item.t<Source.t>>>()    
    let toString = function | PToken y |PLiteral y | PRef (y,_) -> Source.toString y 
                            | _ -> ""   
    List.iter (fun x -> (Set.iter (fun y -> let gt = make_goto (singleton y) x in
#if DEBUG
                                                print_any (y, toString x) ; 
                                                print_any " -> ";
                                                print_any gt;
#endif
                                                t.Add((y, toString x),gt)))items) lex_list;
                       
    t
