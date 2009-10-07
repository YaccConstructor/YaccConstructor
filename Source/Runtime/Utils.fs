// Utils.fs
//
// Copyright 2009 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

#light

module Yard.Core.Utils

open IL
open Production
open Grammar.Item

let next = 
    let i = ref 0
    let next() = incr i;!i
    next  

let getText = function
    |Some(x) -> x  
    | _                              -> "" 

let mgetText x = 
    match x with
    |PLiteral(y)|PToken(y)|PRef(y,_)-> Source.toString y
    |_                    -> ""

let prevItem item items = 
    let isPrev x = Some item.item_num = x.next_num && item.prod_num = x.prod_num
    Set.filter isPrev items
    
let nextItem item items = 
    let isNext x = item.next_num = Some x.item_num && item.prod_num=x.prod_num
    Set.filter isNext items    
