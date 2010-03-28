// AbstractTable.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Core.Tables

open Yard.Core

[<AbstractClass>]
type Table<'a>(fileName) = 
    
    let fileExtension = ".dta"
        
    abstract fileSuffix: string
       
    abstract FullFileName: string with get
    default this.FullFileName = String.concat "." [fileName; this.fileSuffix; fileExtension]
   
    abstract member Load: Unit->'a    
    default this.Load() = IO.readValue (this.FullFileName):'a
    
    abstract member Save: 'a->unit
    default this.Save table = IO.writeValue (this.FullFileName) table