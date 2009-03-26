#light

module Data
 
open IO
open IL
open Grammar.Item

let goto_set:System.Collections.Generic.Dictionary<int(*Grammar.Item.t<Source.t>*string*),Set<Grammar.Item.t<Source.t>>> = readValue "goto.dta"

let items:Set<t<Source.t>> = readValue "items.dta"