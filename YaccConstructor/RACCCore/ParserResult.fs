// ParserResult.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Generators.RecursiveAscent
[<Struct>]
type ParserResult<'symb,'leafVal,'nodeVal, 'trace when 'leafVal : equality and 'leafVal : comparison and 'symb :equality > = 
 val position : int;
 val state : State<'symb,'leafVal,'nodeVal,'trace>; 
 override self.ToString() = self.state.ToString()  
 new (state,position) = {state=state; position=position}
