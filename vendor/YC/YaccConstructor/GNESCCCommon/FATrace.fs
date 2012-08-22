//  FATrace.fs contains type definition for labels for finite automata transitions.
//
//  Copyright 2009,2010,2011 Semen Grigorev <rsdpisuy@gmail.com>
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software:you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

namespace Yard.Generators.GNESCCGenerator

type FATrace =     
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
    | TOptS of int
    | TOptE of int
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
        | TOptS  x -> "(TOptS " + x.ToString() + ")"
        | TOptE  x -> "(TOptE " + x.ToString() + ")"
   