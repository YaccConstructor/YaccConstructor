// Data.fs
//
// Copyright 2009 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

#light

module Data 

open IL
open Production

let (get_next_ch:int->t<string,string>),input_length =       
    let lex_list = ref Test.test3                          
    let l = List.length !lex_list 
    let get i =  List.nth (!lex_list) (l-i)        
    let input_length () = l 
    get,input_length           

let goto_set:System.Collections.Generic.Dictionary<int,Set<Grammar.Item.t<Source.t>>> = 
      let dict = new System.Collections.Generic.Dictionary<int,Set<Grammar.Item.t<Source.t>>>() in
      let rv:System.Collections.Generic.List<_> = IO.readValue "goto.dta" in      
          for (k_v_pair:System.Collections.Generic.KeyValuePair<int,Set<Grammar.Item.t<Source.t>>>)
           in (rv.ToArray()) do  dict.Add(k_v_pair.Key,k_v_pair.Value)
      dict   
  
let items:Set<Grammar.Item.t<Source.t>> = IO.readValue "items.dta"

let start_nterms:List<string> = IO.readValue "start_nterms.dta" 