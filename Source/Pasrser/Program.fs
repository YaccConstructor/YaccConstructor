//Григорьев Семён. Рекурсивно всходящий анализатор. Работает  с арифмитическими выражениями
// E->E+E
// E->E*E
// E->(E)
// E-> a

//Как видно, грамматика неоднознаная.


#light "off"

open Set
open System
open Log

type Symb = 
     |Term of char
     |NTerm of char
     
type ISymb  = 
     |Term of char
     |NTerm of char
     |Dot   of char*int // 0 -> NTrem; 1-> Term
     
type rule = Rule of  char *(Symb list)

type item = Item of char*(ISymb list)

let lex_list = ['E';'a';'+';'*';' ';'S';')';'(']

let items = 
    [Set.of_list[Item('S',[Dot('E',0)])]
    ;Set.of_list[Item('E',[Dot('E',0);Term('+');NTerm('E')])]
    ;Set.of_list[Item('E',[NTerm('E');Dot('+',1);NTerm('E')])]
    ;Set.of_list[Item('E',[NTerm('E');Term('+');Dot('E',0)])]
    ;Set.of_list[Item('E',[NTerm('E');Term('+');NTerm('E');Dot(' ',1)])]
    ;Set.of_list[Item('E',[Dot('E',0);Term('*');NTerm('E')])]
    ;Set.of_list[Item('E',[NTerm('E');Dot('*',1);NTerm('E')])]
    ;Set.of_list[Item('E',[NTerm('E');Term('*');Dot('E',0)])]
    ;Set.of_list[Item('E',[NTerm('E');Term('*');NTerm('E');Dot(' ',1)])]
    ;Set.of_list[Item('E',[Dot('a',1)])]
    ;Set.of_list[Item('E',[Term('a');Dot(' ',1)])]
    ;Set.of_list[Item('E',[Dot('(',1);NTerm('E');Term(')')])]
    ;Set.of_list[Item('E',[Term('(');Dot('E',0);Term(')')])]
    ;Set.of_list[Item('E',[Term('(');NTerm('E');Dot(')',1)])]
    ;Set.of_list[Item('E',[Term('(');NTerm('E');Term(')');Dot(' ',1)])]
    ]

let Q = Set.union_all (Set.of_list[Item('S',[NTerm('E');Dot(' ',1)])]::items)


let getText a (x,y,z)= 
    match a 
    with 
    |Term(t) -> if x = 0 then ' ' else t 
    |NTerm(t)-> if y = 0 then ' ' else t
    |Dot(t,i)  -> if z = 0 then ' ' else t 
    
    
//запоминалка. Используем для запоминания результатов ф-ий parse и climb   
let memoize (f: 'a ->'b) =
let t = new System.Collections.Generic.Dictionary<'a,'b>() 
in
fun a ->    
    if t.ContainsKey(a)
    then t.[a]
    else 
    let res = f a 
    in
    t.Add(a,res);
    res    
    
let closure = 
    memoize(fun q -> 
    let ex = new System.Collections.Generic.KeyNotFoundException()
    in
    let rec cl i q = 
        if i = Set.count q 
        then q
        else
         let next_cl f = cl (i+1) (union_all [q;filter f Q])
         in 
         next_cl (fun x -> 
                      let el_for_cl = (List.nth (Set.to_list q) i)
                      in  
                      let getDot lst = List.find (function Dot(_,0)->true| _ -> false ) lst
                      in                             
                      match x 
                      with 
                      | Item (b,hd1::tl1) -> (fun (Item(a,lst)) -> 
                                                  try(b = (getText (getDot lst) (0,0,1)) && 
                                                      (function Dot(_)->true | _ -> false) hd1)
                                                  with ex -> false)                                                           
                                              el_for_cl
                      |_ -> false                     
                  )
    in
    cl 0 q)

let rec next dlst = 
    match dlst
    with
    | Dot(a,0)::NTerm(x)::tl -> NTerm(a)::Dot(x,0)::tl
    | Dot(a,1)::NTerm(x)::tl -> Term(a)::Dot(x,0)::tl
    | Dot(a,0)::Term(x)::tl -> NTerm(a)::Dot(x,1)::tl
    | Dot(a,1)::Term(x)::tl -> Term(a)::Dot(x,1)::tl
    | Dot(a,1)::[] -> Term(a)::Dot(' ',1)::[]
    | Dot(a,0)::[] -> NTerm(a)::Dot(' ',1)::[]
    | hd::tl -> hd::(next tl)
    | [] -> []
    
let rec prev dlst = 
    match dlst
    with
    | Term(x)::Dot(' ',1)::tl -> Dot(x,1)::tl
    | NTerm(x)::Dot(' ',1)::tl-> Dot(x,0)::tl
    | NTerm(x)::Dot(a,0)::tl  -> Dot(x,0)::NTerm(a)::tl
    | NTerm(x)::Dot(a,1)::tl  -> Dot(x,0)::Term(a)::tl
    | Term(x)::Dot(a,0)::tl   -> Dot(x,1)::NTerm(a)::tl
    | Term(x)::Dot(a,1)::tl   -> Dot(x,1)::Term(a)::tl
    | (Dot(a,_)::[])as x      -> x                            
    | hd::tl                  -> hd::(prev tl)
    | []                      -> []                             
                           
  
let closure_set = 
    let t = System.Collections.Generic.Dictionary<item,Set<item>>()
    in
    Set.map (fun x -> t.Add(x,closure (Set.add  x empty)))Q;
    t

let goto_set = 
    let make_goto q x =     
    let cl = union_all (Set.map (fun x -> closure_set.[x]) q)
    in 
    Set.map (fun (Item(a,lst)) -> Item(a,next lst))
            (Set.filter (fun (Item(a,lst)) -> (List.exists (fun y -> (x = getText y (0,0,1))) lst) ) cl)
    in
    let t = System.Collections.Generic.Dictionary<(item*char),Set<item>>() 
    in
    List.map (fun x-> (Set.map (fun y-> t.Add((y,x),(make_goto (add y empty)) x)))Q) lex_list;
    t  
           
let goto (q,x) = union_all (Set.map (fun y -> goto_set.[(y,x)])q )
                       
let ItNext (Item(a,lst)) = Item(a,next lst)
let ItPrev (Item(a,lst)) = Item(a,prev lst)
                 
let rec climb =
    memoize (fun (q,x,i) -> 
    //print_climb_1 i x q;
    if q = empty
    then empty
    else
    let gt =  goto (q,x)
    in
    //print_climb_2 gt;
    let new_q = parse (gt,i)
    in 
    let union_from_Some set = Set.map (fun (Some(x)) -> x)(Set.filter ((<>)None)(of_list set))
    in                        
    //print_climb_3 new_q;
    if Set.exists (fun x->x = (Item('S',[NTerm('E');Dot(' ',1)]),[]))new_q
    then new_q
    else    
    Set.union_all
    [ union_from_Some[for x1 in new_q -> 
                          if Set.exists (fun (Item(ch,lst) as y) -> 
                                             (ItNext y) = (fst x1)
                                             &&(function Dot(_)::tl-> false
                                                        |[]        -> false
                                                        | _        -> true)lst
                                          )q
                           then Some((ItPrev (fst x1)),snd x1)
                           else None]
    ;
    Set.union_all(
    union_from_Some[for (Item(a,hd::tl),str) in new_q -> 
                        if getText hd (1,1,1)=x && not(a='S')&&(function Dot(_)::tl->true                                                             
                                                                        | _        -> false)tl
                        then Some(climb (q, a, str))
                        else None])
    ])                

and parse = 
    memoize (fun (q,i) -> 
    //print_parse q i;
    let m_fun lst = union_all (of_list (List.map (fun (Some(x)) -> x)(List.filter((<>)None) lst)))
    in                
    union_all
        [map (fun x -> (x,i))(Set.filter (fun(Item(a,lst))-> (List.hd (List.rev lst)) = Dot(' ',1))q)
         ;if i = [] then empty else climb (q,  (List.hd i), (List.tl i))
         ;m_fun [for (Item(a,lst)) in Q -> match lst 
                                           with
                                           | NTerm('`')::[]->Some (climb (q, a, i))
                                           | _             ->None]
         ])
         
// БАГА!!!!         
let p =  System.DateTime.UtcNow        
let c = parse (items.Head, 
//ниже - правильная строка 09.12.2008 разобрал за 1мин.45сек.(уже 20 сек.) (И это вместо нескольких десятков минут!!!)
//  ['a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'+';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'+';'(';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'+';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'+';'(';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';')'] 
//ниже - правильная строка 09.12.2008 разобрал за примерно 45мин.
['a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'+';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'+';'(';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'+';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'+';'(';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';')';'+';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'+';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'+';'(';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'+';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'+';'(';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';')';'+';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'+';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'+';'(';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'+';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'+';'(';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';')';'+';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'+';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'+';'(';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'+';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'+';'(';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';')']  
//ниже - неправильная строка
//['a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'+';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'+';'(';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'+';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'a';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';'+';'(';'a';'*';'a';'*';'(';'a';'+';'a';')';'*';'+';'a';'*';'a';'*';'(';'a';'+';'a';')';')']  
 )
//незабудь исправить багу!!
//сейчас ему пофиг, на то, какого типа параметр Х в climb. Это неверно!!!!
     
do print_any p ;Console.WriteLine("\n Result ::: \n");print_any(c);print_any(System.DateTime.UtcNow);(Console.ReadLine();())        