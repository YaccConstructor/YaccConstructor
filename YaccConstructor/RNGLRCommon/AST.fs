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
    /// Or 1 << 31 ^ term_num, if it's terminal.
    ruleNumber : int;
    /// Children of root
    children : MultiAST [];
}
/// For one nonTerminal there can be a lot of dirivation trees
and MultiAST = AST list ref

type ASTTyper =
    static member private flag = 1 <<< 31
    static member private emptyArray = [| |]
    static member isTerminal (a : AST) = ((a.ruleNumber &&& ASTTyper.flag) <> 0) && (a.ruleNumber <> -1)
    static member isNonTerminal (a : AST) = (a.ruleNumber &&& ASTTyper.flag) = 0
    static member isEpsilon (a : AST) = a.ruleNumber = -1

    static member getRuleNumber (a : AST) = a.ruleNumber
    static member getTermIndex (a : AST) = a.ruleNumber ^^^ ASTTyper.flag

    static member createEpsilonTree nTerm = ref [{ruleNumber = nTerm; children = [| ref [{ruleNumber = -1; children = ASTTyper.emptyArray}] |]}]
    static member createTerminalTree term = ref [{ruleNumber = term; children = ASTTyper.emptyArray}]
