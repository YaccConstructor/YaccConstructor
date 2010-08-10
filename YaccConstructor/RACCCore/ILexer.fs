// ILexer.fs
//
// Copyright 2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.


namespace Yard.Generators.RecursiveAscent

type Lexeme<'a> = {
    name : string;
    value: 'a;     
  }
 
type ILexer = interface
    abstract Next : unit -> Lexeme<_>
end