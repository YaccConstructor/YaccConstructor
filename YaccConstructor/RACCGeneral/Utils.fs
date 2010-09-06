// Utils.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

module Yard.Generators.RecursiveAscent.Utils

open Yard.Core.IL
open Yard.Core.IL.Production
open Grammar.Item

type Enumerator() = class
    let i = ref -1
    let next() = incr i; !i          
    member self.Next() = next()
    member self.Reset() = i := -1   
end
    
let prevItem item items = 
    let isPrev x = Some item.item_num = x.next_num && item.prod_num = x.prod_num
    Set.filter isPrev items
    
let nextItem item items = 
    let isNext x = item.next_num = Some x.item_num && item.prod_num=x.prod_num
    Set.filter isNext items