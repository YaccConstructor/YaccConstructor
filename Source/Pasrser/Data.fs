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

let goto_set:System.Collections.Generic.IDictionary<int,Set<Grammar.Item.t<Source.t>>> = IO.readValue "goto.dta"

let items:Set<Grammar.Item.t<Source.t>> = IO.readValue "items.dta"