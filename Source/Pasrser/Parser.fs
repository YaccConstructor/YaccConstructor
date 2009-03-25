#light 
#nowarn "40"
open IL
open Production
open Grammar.Item
open Tree
open Set
open Data
open Utils

let m_end,m_start = (PLiteral("$",(1,1)),PToken("S",(1,1)))

let start_time = ref System.DateTime.Now                                   
                 
let _end = [0;4;7;8;9]    

let memoize (f: ('a*'c) ->'b) =
   let t = new System.Collections.Generic.Dictionary<Set<'x>*'c,'b>()   
   fun (x,y) ->        
       let id = hash(x);
       System.Console.WriteLine ();
       System.Console.WriteLine ("In dictionary");
       //print_any "Hash key: ";
       //print_any id;
       //System.Console.WriteLine ();
       //print_any "Value key: ";
       //print_any x;
       System.Console.WriteLine ();
       //print_any "Finding result: ";
       let key = x
                //Set.map (fun ((state,treelst)) -> state) x 
                //hash(Set.map (fun ((state,treelst)) -> state,(List.map (fun (Node(_,_,_,num)|Leaf(_,_,num)|RefTo(num))->num) treelst)) x )
       if t.ContainsKey((key,y))       
       then 
         (print_any" RESULT_FIND";
          let res = t.[key,y]
          //res)
          Set.map(fun ((state,treelst),i)->
                      ((state,treelst),i)
                      //((state,(List.map (fun (Node(_,_,_,num)|Leaf(_,_,num)|RefTo(num))->RefTo(num)) treelst)),i)
                  )res)
       else 
         (print_any "RESULT_NOT_FIND";
          let res = f (x,y) 
          t.Add((key,y),res);          
          res )   
                      
do start_time := System.DateTime.Now;
   printfn "Closure and goto calculation.\nStart time: %A" System.DateTime.Now    

let goto (states,symbol) =  Set.union_all (Set.map (fun (y,tree) -> 
                                                    Set.map(fun z -> (z,tree))(goto_set.[(y,symbol)]))states )                         
   
let union_from_Some set = set |> List.filter Option.is_some |> List.map Option.get |> Set.of_list                              
   

let rec climb =
    memoize (fun (states,(symbol,i)) -> 
#if DEBUG    
    //Log.print_climb_1 i symbol states;
#endif
    if states = Set.empty
    then Set.empty
    else     
    let gt =  goto (states,symbol)    
#if DEBUG
    //Log.print_climb_2 gt;    
#endif
    let new_states = parse (gt,i)   
#if DEBUG
    //Log.print_climb_3 new_states;    
#endif             
    if Set.exists (fun ((x,tree),x2)-> x.prod_name="S"&&x.next_num=None&&x2=1) new_states     
    then map (fun a -> a,1) (filter (fun (a,b)-> a.next_num = None) states)
    else    
    Set.union_all [Set.filter (fun (items,i)-> 
                   Set.exists (fun (item,tree)  ->                                    
                                   (exists ((=) (fst items))(nextItem item) )&&(item.item_num <> item.s)
                               )states)new_states
     |>(Set.map (fun (items,i)->(map (fun itm -> ((itm,((snd (Set.choose states))@(snd items))),i))(prevItem (fst items)))))|>union_all    
    ;
    Set.union_all(
    union_from_Some[for ((item,tree),i) in new_states -> 
                        if (exists (fun itm -> (getText itm.symb) = symbol) (prevItem item))
                            && 
                           (item.prod_name<>"S")
                            &&
                           (exists (fun itm -> itm.item_num=item.s)(prevItem item))
                        then (Some((climb (Set.map (fun (state,_tree) -> (state, [(Node(_tree@tree,item.prod_name,[],1))])) states,(item.prod_name,i)))))
                        else None])
    ])                

and parse =           
    memoize (fun (states,i) -> 
#if DEBUG 
    //Log.print_parse states i;
#endif
    let text = mgetText(get_next_ch i)    
    let empty_tree = []
    let leaf_tree = [(Leaf(text,[],1))]
    let new_states = Set.filter (fun (item,tree) -> (item.next_num=None))states
    let result_states states create_tree = Set.map (fun (item,tree) -> (item,(create_tree))) states
    union_all
        [map (fun x -> x,i)(result_states new_states empty_tree)
         ;if (get_next_ch i = m_end) then empty else climb(result_states states leaf_tree,(text,i-1))        
         ])
                 
let res x =
    let parse_res =parse (of_list ((List.map (fun x -> (x,[]))(List.filter (fun x -> x.prod_name ="S")(Set.to_list items)))),input_length()) 
    (function res_s-> iter(fun ((a,b),i)->List.iter print_tree b) res_s) parse_res;
    not(parse_res=empty)
do                    
   let r = res ()
   printfn "Result : %A" r;
   printfn "End time: %A Total: %A" System.DateTime.Now (System.DateTime.Now - (!start_time));
   ignore(System.Console.ReadLine())
