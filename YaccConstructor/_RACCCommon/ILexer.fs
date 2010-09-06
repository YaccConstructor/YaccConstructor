// ILexer.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Generators.RACC

type Lexeme<'value> = {
    name : string;
    value: 'value;     
  }
 
type ILexer<'lexemeValue> = interface
    abstract Next : unit -> Lexeme<'lexemeValue>
end