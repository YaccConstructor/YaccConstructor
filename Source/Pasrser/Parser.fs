#light 
#nowarn "40"
open IL
open Production
open Grammar.Item
open Tree
open Set
open Data
open Utils

open System.Threading

let m_end,m_start = (PLiteral("$",(1,1)),PToken("S",(1,1)))

let start_time = ref System.DateTime.Now
let end_time   = ref System.DateTime.Now                     

let all_in_work = ref false

let count = ref 0 

let memoize (f: ('a*'c) ->'b) =
   let t = new System.Collections.Generic.Dictionary<Set<'x>*'c,'b>()   
   fun (x,y) ->        
       let id = hash(x);
       let key = x,y
       if t.ContainsKey(key)       
       then 
         (
          let res = t.[key]
          res)
       else 
         (
          let res = f (x,y) 
          t.Add(key,res);          
          res )   
                      
do start_time := System.DateTime.Now;
   printfn "Parsing.\nStart time: %A" System.DateTime.Now    

let goto (states,symbol) = union_all [for y,tree in states -> set[for z in (goto_set.[(hash (y,symbol))]) -> (z,tree)]]   
   
let rec climb =
    memoize (fun (states,(symbol,i)) -> 
    if states = Set.empty
    then Set.empty
    else     
    let gt =  goto (states,symbol)    
    let new_states = parse (gt,i)   
#if DEBUG
    Log.print_climb_1 i symbol states;
    Log.print_climb_2 gt;
    Log.print_climb_3 new_states;    
#endif             
    if Set.exists (fun ((x,tree),x2) -> x.prod_name="S"&&x.next_num=None&&x2=1) new_states     
    then set [for state in states do if (fst state).next_num = None then yield state,1] 
    else     
      union_all          
        [for (item,tree) as items, i in new_states do                        
            if (let previousItems = prevItem item
                exists (fun itm -> (getText itm.symb) = symbol) previousItems
                && item.prod_name <> "S"
                && exists (fun itm -> itm.item_num=item.s)previousItems)
            then yield climb(map (fun (state,_tree) -> state, [Node(_tree@tree,item.prod_name,[],1)]) states
                            ,(item.prod_name,i))
            else
                if exists (fun (item,_) -> (exists ((=)(fst items))(nextItem item))&&(item.item_num <> item.s))states
                then yield (map (fun itm -> (itm,(snd (choose states))@(snd items)),i)(prevItem (fst items)))]                
    )                

and parse =           
    memoize (fun (states,i) -> 
#if DEBUG 
    Log.print_parse states i;
#endif
    let text = mgetText(get_next_ch i)        
    let leaf_tree = [(Leaf(text,[],1))]
    let new_states = Set.filter (fun (item,tree) -> item.next_num=None)states
    let result_states states create_tree = set[for (item,tree) in states -> item,create_tree]
    let p = ref empty  
    let h = ref empty  
    let f1 _ = p := map (fun x -> x,i)(result_states new_states [])
    let tr1 = new Thread(new ThreadStart(f1));
    map (fun x -> x,i)(result_states new_states [])
    + if (get_next_ch i = m_end) then empty else climb(result_states states leaf_tree,(text,i-1))
    )
                 
let res x = 
    let parse_res =parse (of_list ((List.map (fun x -> (x,[]))(List.filter (fun x -> x.prod_name ="S")(Set.to_list items)))),input_length())
    end_time := System.DateTime.Now;    
    let trees = of_list(List.concat(map(fun ((a,b),i)-> b) parse_res));
    iter(fun b -> print_tree b) trees;
    printfn "Parser get %A dirivation tree" trees.Count;
    not(parse_res=empty)
do                    
   let r = res ()
   printfn "Result : %A" r;
   printfn "End parsing time: %A Total: %A" !end_time (!end_time - (!start_time));
   printfn "End working time: %A Total: %A" System.DateTime.Now (System.DateTime.Now - (!start_time));
   ignore(System.Console.ReadLine())