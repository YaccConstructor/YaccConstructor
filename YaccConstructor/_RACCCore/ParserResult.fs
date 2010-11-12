// ParserResult.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Generators._RACCGenerator

open Yard.Core.CompareHelper

[<CustomEquality; CustomComparison>]
type ParserResult<'item, 'value when 'item : comparison> =
    {
        rItem      : 'item
        rI         : int
        rLexer     : ILexer<'value>
    }
      
     member self.GetValue (x:ParserResult<_,_>) =
        x.rItem, x.rI
     override self.Equals y = equalsOn self.GetValue self y
     override self.GetHashCode() = hashOn self.GetValue self
     interface System.Collections.IStructuralComparable with
            member self.CompareTo (y,c) = c.Compare(self.GetValue self ,self.GetValue (y :?> ParserResult<'item, 'value>))
