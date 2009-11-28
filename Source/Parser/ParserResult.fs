// ParserResult.fs
//
// Copyright 2009 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Core

type ParserResult<'symb,'leafVal,'nodeVal when 'symb : equality and 'symb : comparison> = struct
 val position : int;
 val state : State<'symb,'leafVal,'nodeVal>; 
 new (state,position) = {state=state; position=position}
end