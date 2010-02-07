// Trace.fs
//
// Copyright 2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Core

type Position = First|Second

type Trace = 
| Seq
| Alt of Position
| Closure