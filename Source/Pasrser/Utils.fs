#light

module Utils

open IL
open Production
open Grammar.Item
open Data

let m_next = 
    let i = ref 0
    let m_next () = incr i;!i
    m_next  

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

let getText = function
    |Some( Grammar.Symbol.Terminal x )     ->  Source.toString x  
    | _                                    -> "" 


let prevItem item = 
    let isPrev x = Some item.item_num = x.next_num && item.prod_num = x.prod_num
    Set.filter isPrev items
    
let nextItem item = 
    let isNext x = item.next_num = Some x.item_num && item.prod_num=x.prod_num
    Set.filter isNext items    

    
