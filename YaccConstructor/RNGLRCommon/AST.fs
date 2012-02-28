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

type NodeType = Term | NonTerm | EpsTree

type AST = {
    number : int;
    nodeType : NodeType;
    /// Children of root
    children : MultiAST [];
}
/// For one nonTerminal there can be a lot of dirivation trees
and MultiAST = AST list ref

type ASTTyper =
    static member private flag = 1 <<< 31
    static member private emptyChildren = [| |]
    static member isTerminal (a : AST) = a.nodeType = Term
    static member isNonTerminal (a : AST) = a.nodeType = NonTerm
    static member isEpsilon (a : AST) = a.nodeType = EpsTree

    static member createEpsilonTree nTerm = ref [{number = nTerm; nodeType = EpsTree; children = ASTTyper.emptyChildren}]
    static member createTerminalTree term = ref [{number = term; nodeType = Term; children = ASTTyper.emptyChildren}]
    static member createNonTerminalTree nTerm children = {number = nTerm; nodeType = NonTerm; children = children}
