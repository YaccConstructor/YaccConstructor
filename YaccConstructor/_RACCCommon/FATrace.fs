// FATrace.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Generators._RACCGenerator

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
    override self.ToString () =
        match self with
        | TSmbS  -> "TSmbS"
        | TSmbE  -> "TSmbE"
        | TSeqS  -> "TSeqS"
        | TSeqE  -> "TSeqE"
        | TAlt1S -> "TAlt1S"
        | TAlt1E -> "TAlt1E"
        | TAlt2S -> "TAlt2S"
        | TAlt2E -> "TAlt2E"
        | TClsS  -> "TClsS"
        | TClsE  -> "TClsE"

type FATrace = 
    | Omega
    | FATrace of TStep
    override self.ToString () =
        match self with
        | Omega      -> "Omega"
        | FATrace(s) -> "(FATrace " + s.ToString() + ")"



