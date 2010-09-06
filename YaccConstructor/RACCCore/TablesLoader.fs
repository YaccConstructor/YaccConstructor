// TablesLoader.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Generators.RecursiveAscent
open System.Collections.Generic;

type TablesLoader( gotoSet         : IDictionary<int,Set<Grammar.Item.t<string>>>
                  ,items           : Set<Grammar.Item.t<string>>
                  ,startNterms     : string list
                  ,ruleToActionMap : IDictionary<int,string>) = 
  class
    let gotoSet = gotoSet          
          
    let items = items 

    let startNterms = startNterms
    
    let ruleToActionMap = ruleToActionMap
          
    member self.GotoSet with get() = gotoSet
    member self.Items with get() = items
    member self.StartNterms with get () = startNterms
    member self.RuleToActionMap with get () = ruleToActionMap
  end