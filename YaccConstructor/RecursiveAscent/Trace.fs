// Trace.fs
//
// Copyright 2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Generators.RecursiveAscent

type Position = 
|First
|Second
 override self.ToString() = 
   match self with
   |First  -> "First"
   |Second -> "Second"

type Trace = 
| TSeq
| TAlt of Position
| TClosure
 override self.ToString() = 
   match self with
   | TSeq      -> "TSeq"
   | TAlt(pos) -> "TAlt(" + pos.ToString() + ")"
   | TClosure  -> "TClosure"