// Data.fs
//
// Copyright 2009 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

#light
namespace Yard.Core
open System.Collections.Generic;
open IL
open Production

type Tables(fName: string) = class
    let gotoSet =           
          let kvpList = IO.readValue (fName + ".goto.dta" ): List<KeyValuePair<int,Set<Grammar.Item.t<Source.t>>>>          
          dict <| seq{for kvp in kvpList do yield kvp.Key, kvp.Value}
      
    let items =  IO.readValue (fName + ".items.dta")  : Set<Grammar.Item.t<Source.t>> 

    let startNterms = IO.readValue (fName + ".start_nterms.dta"): string list
    member self.GotoSet with get() = gotoSet
    member self.Items with get() = items
    member self.StartNterms with get () = startNterms
  end