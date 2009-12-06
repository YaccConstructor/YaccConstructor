// TypeConvertableTable.fs
//
// Copyright 2009 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Core.Tables

open Yard.Core
open System.Collections.Generic

[<AbstractClass>]
type TypeConvertableTable<'b,'c  when 'b:equality>(fileName) = 
    inherit Table<List<KeyValuePair<'b,'c>>>(fileName)
    
    abstract ConvertTypeOnSave: IDictionary<'b,'c>  -> List<KeyValuePair<'b,'c>>
    default this.ConvertTypeOnSave table = System.Linq.Enumerable.ToList table 
    
    abstract ConvertTypeOnLoad: List<KeyValuePair<'b,'c>> -> IDictionary<'b,'c>
    default this.ConvertTypeOnLoad kvpList = dict <| seq{for kvp in kvpList do yield kvp.Key, kvp.Value}