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
| TSmbS of int
| TSmbE of int
| TSeqS of int
| TSeqE of int
| TAlt1S of int
| TAlt1E of int
| TAlt2S of int
| TAlt2E of int
| TClosureS of int
| TClosureE of int
 override self.ToString() = 
   match self with
   | TSmbS(n)      -> "TSmbS(" + n.ToString() + ")"
   | TSmbE(n)      -> "TSmbE(" + n.ToString() + ")"
   | TSeqS(n)      -> "TSeqS(" + n.ToString() + ")"
   | TSeqE(n)      -> "TSeqE(" + n.ToString() + ")"
   | TAlt1S(n)     -> "TAlt1S(" + n.ToString() + ")"
   | TAlt1E(n)     -> "TAlt1E(" + n.ToString() + ")"
   | TAlt2S(n)     -> "TAlt2S(" + n.ToString() + ")"
   | TAlt2E(n)     -> "TAlt2E(" + n.ToString() + ")"
   | TClosureS(n)  -> "TClosureS(" + n.ToString() + ")"
   | TClosureE(n)  -> "TClosureE(" + n.ToString() + ")"