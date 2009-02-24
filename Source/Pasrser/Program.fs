//Григорьев Семён. Рекурсивно всходящий анализатор. Работает  с арифмитическими выражениями
// E->E+E
// E->E*E
// E->(E)
// E-> a

//Как видно, грамматика неоднознаная.

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
    //aa+b*(b+ba)+b*(b+ba)....
    //правильная строка
    let test1 = [PLiteral("a",(1,1));PLiteral("a",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("b",(1,1));PLiteral(")",(1,1));PLiteral("$",(1,1))]  
    //неправильная строка
    let test2 = [PLiteral("a",(1,1));PLiteral("a",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("b",(1,1));PLiteral(")",(1,1));PLiteral("$",(1,1))]    
    let test3= [PLiteral("a",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("$",(1,1))]  
    let test4= [PLiteral("a",(1,1));PLiteral("$",(1,1))]   
    let _lex_list = ref test3                          
    let l = List.length !_lex_list 
    let get i =  List.nth (!_lex_list) (l-i)        
    let input_length ()= l 
    get,input_length           
     
let mgetText x = 
    match x with
    PLiteral(y)|PToken(y)-> Source.toString y
    |_ -> ""
//запоминалка. Используем для запоминания результатов ф-ий parse и climb   
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
    
//это предпросчёт goto. сам анализатор тогда работает быстрее. (closure - очень дорогая операция)           
let goto (states,symbol) =  Set.union_all (Set.map (fun (y,tree) -> Set.map(fun z -> (z,tree))(PreCalculation.goto_set.[(y,symbol)]))states )                         
   
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
#if DEBUG
    Log.print_climb_2 gt;    
#endif
    let new_states = parse (gt,i)   
#if DEBUG
    Log.print_climb_3 new_states;    
#endif             
    if Set.exists (fun ((x,tree),x2)-> x.prod_name="S"&&x.next_num=None&&x2=1) new_states     
    then new_states
    else    
    Set.union_all [Set.filter (fun (items,i)-> 
                   Set.exists (fun (item,tree)  -> 
                                   (exists ((=) (fst items)) (PreCalculation.nextItem item) )&&(item.item_num <> item.s)
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
                        then (print_any "Tree: "; print_any tree; Some(climb (Set.map (fun (state,_tree)-> (state,tree@_tree)) states,item.prod_name,i)))
                        else None])
    ])                
and flg = ref true
and parse = 
    if !flg then start_time:=System.DateTime.Now;
    printfn "Start time: %A" System.DateTime.Now;
    flg:=false;     
    memoize (fun (states,i) -> 
#if DEBUG 
 (Log.print_parse states i);
#endif
    let text = mgetText(get_next_ch i)
    let tree1 item = Node([Leaf (PreCalculation.getText item.symb)],item.prod_name)
    let tree2 item = Leaf(text)
    let new_states = Set.filter (fun (item,tree) -> (item.next_num=None))states
    let result_states states _tree = Set.map (fun (item,tree) -> (item,(_tree item)::tree)) states
    union_all
        [map (fun x -> x,i)(result_states new_states tree1)
         ;if (get_next_ch i = m_end) then empty else  climb(result_states states tree2,text,i-1)        
         ])
                 
let res x = not(parse (of_list ((List.map (fun x -> (x,[]))(List.filter (fun x -> x.prod_name ="S")(Set.to_list items)))),input_length())=empty)
do                    
   let r = res ()
   printfn "Result : %A" r;
   printfn "End time: %A Total: %A" System.DateTime.Now (System.DateTime.Now - (!start_time));
   ignore(System.Console.ReadLine())
