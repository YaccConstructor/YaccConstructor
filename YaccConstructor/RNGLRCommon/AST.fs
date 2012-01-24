//  AST.fs contains description of derivation tree.
//
//  Copyright 2011-2012 Avdyukhin Dmitry
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

namespace Yard.Generators.RNGLR

type AST = {
    /// Number of rule, applied to obtain nonTerminal in the root of tree.
    /// Or -1, if it's epsilon-tree.
    /// Or -2, if it's terminal.
    ruleNumber : int;
    /// Children of root
    children : MultiAST[];
}
/// For one nonTerminal there can be a lot of dirivation trees
and MultiAST = AST list

type ASTTyper () =
    let flag = 1 <<< 31
    member this.isTerminal (a : AST) = ((a.ruleNumber &&& flag) <> 0) && (a.ruleNumber <> -1)
    member this.isNonTerminal (a : AST) = (a.ruleNumber &&& flag) = 0
    member this.isEpsilon (a : AST) = a.ruleNumber = -1

    member this.getRuleNumber (a : AST) = a.ruleNumber
    member this.getTermIndex (a : AST) = a.ruleNumber ^^^ flag
