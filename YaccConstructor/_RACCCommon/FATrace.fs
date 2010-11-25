// FATrace.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Generators._RACCGenerator

type TStep = 
    | TSmbS of int
    | TSmbE of int
    | TSeqS of int
    | TSeqE of int
    | TAlt1S of int
    | TAlt1E of int
    | TAlt2S of int
    | TAlt2E of int
    | TClsS of int
    | TClsE of int
    override self.ToString () =
        match self with
        | TSmbS x  -> "(TSmbS " + x.ToString() + ")"
        | TSmbE x  -> "(TSmbE " + x.ToString() + ")"
        | TSeqS x  -> "(TSeqS " + x.ToString() + ")"
        | TSeqE  x -> "(TSeqE " + x.ToString() + ")"
        | TAlt1S x -> "(TAlt1S " + x.ToString() + ")"
        | TAlt1E x -> "(TAlt1E " + x.ToString() + ")"
        | TAlt2S x -> "(TAlt2S " + x.ToString() + ")"
        | TAlt2E x -> "(TAlt2E " + x.ToString() + ")"
        | TClsS  x -> "(TClsS " + x.ToString() + ")"
        | TClsE  x -> "(TClsE " + x.ToString() + ")"

type FATrace = 
    | Omega
    | FATrace of TStep
    override self.ToString () =
        match self with
        | Omega      -> "Omega"
        | FATrace(s) -> "(FATrace " + s.ToString() + ")"



