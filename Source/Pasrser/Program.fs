//Григорьев Семён. Рекурсивно всходящий анализатор. Работает  с арифмитическими выражениями
// E->E+E
// E->E*E
// E->(E)
// E-> a

//Как видно, грамматика неоднознаная.

#light "off"
#nowarn "40"
open IL
open Production
open Grammar.Item

let m_end,m_start = (PLiteral("$",(1,1)),PToken("S",(1,1)))

let start_time = ref System.DateTime.Now                                   
             
let (getL:(int->t<string,string>)),iLength = 
    //aa+b*(b+ba)+b*(b+ba)....
    //правильная строка
    let test1 = [PLiteral("a",(1,1));PLiteral("a",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("b",(1,1));PLiteral(")",(1,1));PLiteral("$",(1,1))]
    in
    //неправильная строка
    let test2 = [PLiteral("a",(1,1));PLiteral("a",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("b",(1,1));PLiteral(")",(1,1));PLiteral("$",(1,1))]
    in
    let test3= [PLiteral("a",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("$",(1,1))]
    in
    let test4= [PLiteral("a",(1,1));PLiteral("$",(1,1))]
    in    
    let _lex_list = ref test4
                        
    in
    let l = List.length !_lex_list in
    let get i =  List.nth (!_lex_list) (l-i) in        
    let iLength ()= l in
    get,iLength           
     
let mgetText x = 
    match x 
    with
    PLiteral(y)|PToken(y)-> Source.toString y
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
    
let items = PreCalculation.items
    
//это предпросчёт goto. сам анализатор тогда работает быстрее. (closure - очень дорогая операция)           
let goto (q,x) =  Set.union_all (Set.map (fun y -> PreCalculation.goto_set.[(y,x)])q )                         
   
let union_from_Some set = set |> List.filter Option.is_some |> List.map Option.get |> Set.of_list                              
   
do printfn "End time: %A Total: %A" System.DateTime.Now (System.DateTime.Now - (!start_time))

let rec climb =
    memoize (fun (q,x,i) -> 
#if DEBUG    
    Log.print_climb_1 i x q;
#endif
    if q = Set.empty
    then Set.empty
    else
    let gt =  goto (q,x)
    in
#if DEBUG
    Log.print_climb_2 gt;    
#endif
    let new_q = parse (gt,i)
    in 
#if DEBUG
    Log.print_climb_3 new_q;    
#endif
    if Set.exists (fun (x,x2)-> x.prod_name = "S" && x.next_num = None && x2 = 1) new_q     
    then new_q
    else    
    Set.union_all                            
    [Set.filter (fun x1-> 
                   Set.exists (fun item  -> 
                                   (Set.exists ((=) (fst x1)) 
                                    (PreCalculation.nextItem item) )&&(item.item_num <> item.s)
                               )q)new_q
     |>(Set.map (fun x1->(Set.map (fun itm -> (itm,snd x1))(PreCalculation.prevItem (fst x1)))))|>Set.union_all
    
    ;
    Set.union_all(
    union_from_Some[for (item,i) in new_q -> 
                        if (Set.exists (fun itm -> (PreCalculation.getText itm.symb) = x) (PreCalculation.prevItem item)) && (item.prod_name<>"S")&&(Set.exists (fun itm -> itm.item_num=item.s)(PreCalculation.prevItem item))
                        then Some(climb (q,item.prod_name,i))
                        else None])
    ])                
and flg = ref true
and parse = 
    if !flg then start_time:=System.DateTime.Now;
    printfn "Start time: %A" System.DateTime.Now;
    flg:=false;     
    memoize (fun (q,i) -> 
#if DEBUG
    Log.print_parse q i;
#endif    
    Set.union_all
        [Set.map (fun x -> (x,i) )(Set.filter (fun item -> (item.next_num=None))q)
         ;if (getL i = m_end) then Set.empty else  climb(q,mgetText(getL i),i-1)
        
         ])
                 
let res str = 
        not(parse (Set.of_list ([List.find (fun x -> x.prod_name ="S")(Set.to_list items)]),iLength())=Set.empty)
 
let test_str1 = "a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))*a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))"

let test_str2 = "a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))*a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))+a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))*a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))"

let test_str3 = "a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))*a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))+a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))*a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))+a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))*a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))+a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))*a+a*a*(a+a)*a+a*a*(a+a)+a*a*(a+a)*a+a*a*(a+a)+a+a*a*(a+a)*a+a*a*(a+a)+(a*a*(a+a)*a+a*a*(a+a))"
     
do 
       let str = 
        test_str1 
   in     
   let r = res(str) in
   printfn "Result : %A" r;
   printfn "End time: %A Total: %A" System.DateTime.Now (System.DateTime.Now - (!start_time))