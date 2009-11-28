// TablesLoader.fs
//
// Copyright 2009 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Core
open System.Collections.Generic;

type Tables(fName: string) = class
    let gotoSet =           
          let kvpList = IO.readValue (fName + ".goto.dta" ): List<KeyValuePair<int,Set<Grammar.Item.t<string>>>>          
          dict <| seq{for kvp in kvpList do yield kvp.Key, kvp.Value}
      
    let items =  IO.readValue (fName + ".items.dta")  : Set<Grammar.Item.t<string>> 

    let startNterms = IO.readValue (fName + ".start_nterms.dta"): string list
    
    let ruleToActionMap = 
        let kvpList = IO.readValue (fName + ".rule_to_action.dta" ): List<KeyValuePair<int,string>>          
        dict <| seq{for kvp in kvpList do yield kvp.Key, kvp.Value}
          
    member self.GotoSet with get() = gotoSet
    member self.Items with get() = items
    member self.StartNterms with get () = startNterms
    member self.RuleToActionMap with get () = ruleToActionMap
  end