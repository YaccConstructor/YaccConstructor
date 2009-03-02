#light 
#nowarn "40"
open IL
open Production
open Grammar.Item
open Tree
open Set

let m_end,m_start = (PLiteral("$",(1,1)),PToken("S",(1,1)))

let start_time = ref System.DateTime.Now                                   
             
let (get_next_ch:(int->t<string,string>)),input_length =       
    let _lex_list = ref Test.test5                          
    let l = List.length !_lex_list 
    let get i =  List.nth (!_lex_list) (l-i)        
    let input_length ()= l 
    get,input_length           
     
let mgetText x = 
    match x with
    PLiteral(y)|PToken(y)-> Source.toString y
    |_ -> ""
//Р·Р°РїР_Р_РёР_Р°Р>РєР°. Р_С_РїР_Р>С_Р·С_РчР_ Р_Р>С_ Р·Р°РїР_Р_РёР_Р°Р_РёС_ С_РчР·С_Р>С_С'Р°С'Р_Р_ С"-РёР№ parse Рё climb   
let memoize (f: 'a ->'b) =
   let t = new System.Collections.Generic.Dictionary<'a,'b>()   
   fun x ->        
       if t.ContainsKey(x)
       then t.[x]
       else 
         let res = f x 
         t.Add(x,res);
         res    
                      
do start_time := System.DateTime.Now;
   printfn "Closure and goto calculation.\nStart time: %A" System.DateTime.Now
    
let items = PreCalculation.items
let rec kill_label lst = (function Label::tl -> kill_label tl| x -> x) lst 
//С_С'Р_ РїС_РчР_РїС_Р_С_С+С'С' goto. С_Р°Р_ Р°Р_Р°Р>РёР·Р°С'Р_С_ С'Р_Р_Р_Р° С_Р°Р+Р_С'Р°РчС' Р+С<С_С'С_РчРч. (closure - Р_С+РчР_С_ Р_Р_С_Р_Р_Р°С_ Р_РїРчС_Р°С+РёС_)           
let goto (states,symbol) =  Set.union_all (Set.map (fun (y,tree) -> Set.map(print_any tree;System.Console.WriteLine();
                                                    fun z -> (z, if exists (fun item -> 
                                                                                ((*List.hd tree <> Label &&*)(item.item_num = z.s)(* && PreCalculation.getText item.symb = symbol*))) 
                                                                                (PreCalculation.prevItem z)
                                                                 then ((function (*(Leaf(_) as x)::Label::tree)->x::Label::tree|*)((Leaf(_) as x)::tree)->x::Label::tree|x->x)tree) 
                                                                 else ((function ((Leaf(_) as x)::Label::tree)-> Label::x::tree| (Leaf(_) as y)::x-> Label::y::x|x->x) tree)))(PreCalculation.goto_set.[(y,symbol)]))states )                         
   
let union_from_Some set = set |> List.filter Option.is_some |> List.map Option.get |> Set.of_list                              
   
do printfn "End time: %A Total: %A" System.DateTime.Now (System.DateTime.Now - (!start_time))

let rec climb =
    memoize (fun (states,symbol,i) -> 
#if DEBUG    
    Log.print_climb_1 i symbol states;
#endif
    if states = Set.empty
    then Set.empty
    else     
    let gt =  goto (states,symbol)
    print_any "Symbol: ";print_any symbol;
#if DEBUG
    //Log.print_climb_2 gt;    
#endif
    let new_states = parse (gt,i)   
#if DEBUG
    //Log.print_climb_3 new_states;    
#endif             
    if Set.exists (fun ((x,tree),x2)-> x.prod_name="S"&&x.next_num=None&&x2=1) new_states     
    then new_states
    else    
    Set.union_all [Set.filter (fun (items,i)-> 
                   Set.exists (fun (item,tree)  -> 
                                   (*print_any "NEXT!!!";iter print_any (PreCalculation.nextItem item));*)
                                   (exists ((=) (fst items))(PreCalculation.nextItem item) )&&(item.item_num <> item.s)
                               )states)new_states
     |>(Set.map (fun (items,i)->(map (fun itm -> ((itm,snd items),i))(PreCalculation.prevItem (fst items)))))|>union_all
    
    ;
    Set.union_all(
    union_from_Some[for ((item,tree),i) in new_states -> 
                        if (exists (fun itm -> (PreCalculation.getText itm.symb) = symbol) (PreCalculation.prevItem item))
                            && 
                           (item.prod_name<>"S")
                            &&
                           (exists (fun itm -> itm.item_num=item.s)(PreCalculation.prevItem item))
                        then ((*print_any "Tree: "; print_any tree;*) Some(climb (Set.map (fun (state,_) -> (state,tree)) states,item.prod_name,i)))
                        else None])
    ])                
and flg = ref true
and parse = 
    if !flg then start_time:=System.DateTime.Now;
    printfn "Start time: %A" System.DateTime.Now;
    flg:=false;     
    memoize (fun (states,i) -> 
#if DEBUG 
    Log.print_parse states i;
#endif
    let text = mgetText(get_next_ch i)
    let rec kill_label lst = (function Label::tl -> tl| x -> x) lst      
    let tree1 item tree = 
        //let subnodes =  
        //if tree<> [] then match (List.hd tree) with Node (_,_,c)|Leaf(_,c) -> print_any c;
        
        let rec to_Label lst buf = 
            match lst with
              hd::tl -> match hd with
                          Label -> buf,(tl)
                        | x     -> to_Label tl (x::buf)
              | [] -> buf,[]
        let (red,n_tree) = to_Label (if tree<>[] then kill_label tree else tree)  []     
        Label::(Node(red,item.prod_name,[item.prod_name]))::n_tree
    let tree2 item tree = ((Leaf(text,[text]))::(*function (Label::tree)-> tree| x-> x*) tree)
    let new_states = Set.filter (fun (item,tree) -> (item.next_num=None))states
    let result_states states _tree = Set.map (fun (item,tree) -> (item,(_tree item tree))) states
    union_all
        [map (fun x -> x,i)(result_states new_states tree1)
         ;if (get_next_ch i = m_end) then empty else climb(result_states states tree2,text,i-1)        
         ])
                 
let res x =
    let parse_res =parse (of_list ((List.map (fun x -> (x,[]))(List.filter (fun x -> x.prod_name ="S")(Set.to_list items)))),input_length()) 
    (function res_s-> map(fun ((a,b),i)->List.map Log.print_tree b) res_s) parse_res;
    not(parse_res=empty)
do                    
   let r = res ()
   printfn "Result : %A" r;
   printfn "End time: %A Total: %A" System.DateTime.Now (System.DateTime.Now - (!start_time));
   ignore(System.Console.ReadLine())
