// FATrace.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Generators.RACC

type TStep = 
    | TSmbS 
    | TSmbE 
    | TSeqS 
    | TSeqE 
    | TAlt1S 
    | TAlt1E 
    | TAlt2S 
    | TAlt2E 
    | TClsS 
    | TClsE

type FATrace = 
    | Omega
    | FATrace of TStep



