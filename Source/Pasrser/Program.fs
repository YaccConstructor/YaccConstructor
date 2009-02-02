//Григорьев Семён. Рекурсивно всходящий анализатор. Работает  с арифмитическими выражениями
// E->E+E
// E->E*E
// E->(E)
// E-> a

//Как видно, грамматика неоднознаная.

#light "off"
#nowarn "40"

open Set
open System
open Log
open PreCalculation
open IL.Production
open IL.Source
open IL
open Grammar.Item
open Grammar.Symbol

// debug = true - печатается трасса. Иначе нет.
let debug = true

//interacive = true - ввод строки с консоли. иначе - явная подстановка тестовой строки
let interacive = false

let m_end,m_start = (PLiteral("$",(1,1)),PToken("S",(1,1)))

let start_time = ref System.DateTime.Now                                   
             
let (getL:(int->t<string,string>)),iLength = 
    let _lex_list = ref [PLiteral("a",(1,1));PLiteral("$",(1,1))]//(Seq.to_list //"a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))*a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))$")
                                      //"a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))*a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))+a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))*a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))$")
                          //            "a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))*a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))+a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))*a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))+a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))*a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))+a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))*a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a+*(a+a))$")
                                      //"a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))*a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))+a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))*a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))+a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))*a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))+a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))*a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))+a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))*a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))+a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))*a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))+a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))*a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))+a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))*a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))$")
    in
    let l = List.length !_lex_list in
    let get i =  List.nth (!_lex_list) (l-i) in        
    let iLength ()= l in
    get,iLength           
     
let mgetText x = 
    match x 
    with
    PLiteral(y)|PToken(y)-> toString y
    |_ -> ""
//запоминалка. Используем для запоминания результатов ф-ий parse и climb   
let memoize (f: 'a ->'b) =
let t = new System.Collections.Generic.Dictionary<'a,'b>() 
in
fun a ->        
    if t.ContainsKey(a)
    then t.[(a)]
    else 
    let res = f a 
    in
    t.Add((a),res);
    res    
                      
do start_time := System.DateTime.Now;
   printfn "Closure and goto calculation.\nStart time: %A" System.DateTime.Now
    
//это предпросчёт goto. сам анализатор тогда работает быстрее. (closure - очень дорогая операция)           
let goto (q,x) = try union_all (Set.map (fun y -> goto_set.[(y,x)])q ) with _ -> empty                        
   
let union_from_Some set = set |> List.filter Option.is_some |> List.map Option.get |> Set.of_list                              
   
do printfn "End time: %A Total: %A" System.DateTime.Now (System.DateTime.Now - (!start_time))

let rec climb =
    memoize (fun (q,x,i) -> 
    if debug then print_climb_1 i x q;
    if q = empty
    then empty
    else
    let gt =  goto (q,x)
    in
    if debug then print_climb_2 gt;    
    let new_q = parse (gt,i)
    in 
    if debug then print_climb_3 new_q;
    if Set.exists (fun (x,x2)->x.prod_name="S"&&x.next_num=None) new_q
    then new_q
    else    
    Set.union_all                            
    [Set.filter (fun x1-> 
                   Set.exists (fun item  -> 
                                   (nextItem item = fst x1)&&(item.next_num <> None)
                               )q)new_q
     |>Set.map (fun x1->((prevItem (fst x1),snd x1)))                      
    
    ;
    Set.union_all(
    union_from_Some[for (item,i) in new_q -> 
                        if (getText (prevItem item).symb) = x && (item.prod_name<>"S")&&item.item_num=item.s+1
                        then Some(climb (q,item.prod_name,i))
                        else None])
    ])                

and parse = 
    memoize (fun (q,i) -> 
    if debug  then print_parse q i;    
    union_all
        [map (fun x -> (x,i) )(Set.filter (fun item -> (item.next_num=None))q)
         ;if (getL i = m_end) then empty else  climb(q,mgetText(getL i),i-1)
         (*;union_from_Some[for item in items -> if item.symb = None 
                                               then Some (climb (q,item.prod_name,i))
                                               else None]|> union_all *)
         ])
                 
let res str = 
    start_time:=System.DateTime.Now;
    printfn "Start time: %A" System.DateTime.Now;
    not(parse (of_list ([List.find (fun x -> x.prod_name ="S")(Set.to_list items)]),iLength())=empty)
 
let test_str1 = "a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))*a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))"

let test_str2 = "a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))*a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))+a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))*a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))"

let test_str3 = "a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))*a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))+a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))*a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))+a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))*a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))+a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))*a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))"
     
do 
       let str = 
       if interacive
       then 
        (Console.WriteLine("Insert string:"); 
        Console.ReadLine())
       else
        test_str1 
   in     
   let r = res(str) in
   printfn "Result : %A" r;
   Console.WriteLine();
   printfn "End time: %A Total: %A" System.DateTime.Now (System.DateTime.Now - (!start_time));   
   Console.ReadLine()|>ignore