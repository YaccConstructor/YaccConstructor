#light

module Data 

open IL

let goto_set:System.Collections.Generic.IDictionary<int,Set<Grammar.Item.t<Source.t>>> = IO.readValue "goto.dta"

let items:Set<Grammar.Item.t<Source.t>> = IO.readValue "items.dta"